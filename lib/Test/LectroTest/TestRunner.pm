package Test::LectroTest::TestRunner;

use strict;
use warnings;

use Carp;
use Data::Dumper;

use Test::LectroTest::Property qw( NO_FILTER );

=head1 NAME

Test::LectroTest::TestRunner - Configurable Test::Harness-compatible engine for running LectroTest property checks

=head1 SYNOPSIS

 use Test::LectroTest::TestRunner;

 my @args = trials => 1_000, retries => 20_000;
 my $runner = Test::LectroTest::TestRunner->new(@args);

 # test a single property and print details upon failure
 my $result = $runner->run( $a_single_lectrotest_property );
 print $result->details unless $result->success;

 # test a suite of properties, w/ Test::Harness output
 my $all_successful = $runner->run_suite( @properties );
 print "Splendid!" if $all_successful;

=head1 DESCRIPTION

For now, look at L<LectroTest::Property/SYNOPSIS> for
a common manual usage of TestRunner.


=cut

our %defaults = ( trials  =>  1_000,
                  retries => 20_000,
                  scalefn => sub { $_[0] / 2 + 1 },
                  number  => 1,
                  verbose => 1
);

# build field accessors

for my $field (keys %defaults) {
    no strict 'refs';
    *{$field} = sub {
        my $self = shift;
        $self->{$field} = $_[0] if @_;
        $self->{$field}
    };
}

sub new {
    my $self   = shift;
    my $class  = ref($self) || $self;
    return bless { %defaults, @_ }, $class;
}


sub run {
    local $" = " & ";
    my ($self, $test, $number) = @_;

    # if a test number wasn't provided, take the next from our counter

    unless (defined $number) {
        $number = $self->number;
        $self->number( $number + 1);
    }

    # create a new results object to hold our results; run trials

    my ($gen_specs, $testfn, $name) = @$test{qw/inputs test name/};
    my $results = Test::LectroTest::TestRunner::results->new(
        name => $name, number => $number
    );
    my @vars = sort keys %$gen_specs;
    my $retries = 0;
    my %labels;
    my $base_size = 0;
    for (1 .. $self->trials) {
        $base_size++;
        my $controller = Test::LectroTest::TestRunner::testcontroller->new();
        my $size = $self->scalefn->($base_size);
        my $inputs = { map {($_, $gen_specs->{$_}->generate($size))} @vars  };
        my $success = eval { $testfn->($controller, @$inputs{@vars}) };
        $results->exception( do { my $ex=$@; chomp $ex; $ex } ) if $@;
        if ($controller->retried) {
            $retries++;
            if ($retries >= $self->retries) {
                $results->add_notes("$retries retries exceeded");
                $results->attempts( $_ );
                return $results;
            }
            redo;
        }
        if ($controller->labels) {
            my @cl = sort @{$controller->labels};
            $labels{"@cl"}++ if @cl;
        }
        unless ( $success ) {
            $results->counterexample_( $inputs );
            $results->attempts( $_ );
            return $results;
        }
    }
    $results->success(1);
    $results->attempts( $self->trials );
    $results->labels( \%labels );
    return $results;
}

sub run_suite {
    local $| = 1;
    my $self = shift;
    my @tests;
    my @opts;
    while (@_) {
        if (ref $_[0]) {  push @tests, shift;       }
        else           {  push @opts, shift, shift; }
    }
    my %opts = (verbose => $self->verbose, @opts);
    my $verbose = $opts{verbose};
    $self->number(1);  # reset test-number count
    my $success = 1;   # assume success
    print "1..", scalar @tests, "\n";
    for (@tests) {
        my $results = $self->run($_);
        print $verbose ? $results->details : $results->summary ."\n";
        $success &&= $results->success;
    }
    return $success;
}

package Test::LectroTest::TestRunner::results;
use Class::Struct;
import Data::Dumper;

struct( name            => '$',
        success         => '$',
        labels          => '$',
        counterexample_ => '$',
        exception       => '$',
        attempts        => '$',
        notes           => '$',
        number          => '$',
);

sub summary {
    my $self = shift;
    my ($name, $attempts) = ($self->name, $self->attempts);
    my $notes = $self->notes;
    my $number = $self->number || 1;
    local $" = " / ";
    return $self->success
        ? "ok $number - '$name' ($attempts attempts)"
        : $notes ? "not ok $number - '$name' incomplete (@$notes)"
                 : "not ok $number - '$name' falsified in $attempts attempts";
}

sub add_notes {
    my $self = shift;
    my $notes = $self->notes || [];
    push @$notes, @_;
    $self->notes( $notes );
}

sub details {
    my $self = shift;
    my $summary = $self->summary . "\n";
    my $details .= $self->label_frequencies;
    my $cx = $self->counterexample;
    if ( $cx ) {
        $details .= "Counterexample:\n$cx";
    }
    my $ex = $self->exception;
    if ( $ex ) {
        local $Data::Dumper::Terse = 1;
        $details .= "Caught exception: " . Dumper($ex);
    }
    $details =~ s/^/\# /mg if $details;  # mark as Test::Harness comments
    return "$summary$details";
}

sub label_frequencies {
    my $self = shift;
    my $l = $self->labels || {} ;
    my $total = $self->attempts;
    my @keys = sort { $l->{$b} <=> $l->{$a} } keys %$l;
    join( "\n",
          (map {sprintf "% 3d%% %s", (200*$l->{$_}+1)/(2*$total), $_} @keys),
          ""
    );
}

sub counterexample {
    my $self = shift;
    my $vars = $self->counterexample_;
    return "" unless $vars;  # no counterexample
    my $sorted_keys = [ sort keys %$vars ];
    my $dd = Data::Dumper->new([@$vars{@$sorted_keys}], $sorted_keys);
    $dd->Sortkeys(1);
    return $dd->Dump;
}


package Test::LectroTest::TestRunner::testcontroller;
import Class::Struct;

struct ( labels => '$', retried => '$' );

sub retry {
    shift->retried(1);
}

sub label {
    my $self = shift;
    my $labels = $self->labels || [];
    push @$labels, @_;
    $self->labels( $labels );
}

sub trivial {
    shift->label("trivial");
}

package Test::LectroTest::TestRunner;

1;



=head1 LECTROTEST HOME

The LectroTest home is
L<http:E<sol>E<sol>community.moertel.comE<sol>LectroTest>.  There you
will find more documentation, presentations, a wiki, and other helpful
LectroTest-related resources.  It's also the best place to ask
questions.

=head1 AUTHOR

Tom Moertel (tom@moertel.com)

=head1 INSPIRATION

The LectroTest project was inspired by Haskell's fabulous
QuickCheck module by Koen Claessen and John Hughes:
L<http:E<sol>E<sol>www.cs.chalmers.seE<sol>~rjmhE<sol>QuickCheckE<sol>>.

=head1 COPYRIGHT and LICENSE

Copyright 2004 by Thomas G Moertel.  All rights reserved.

This program is free software; you can redistribute it and/or
modify it under the same terms as Perl itself.

=cut
