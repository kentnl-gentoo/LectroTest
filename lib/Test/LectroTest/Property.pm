package Test::LectroTest::Property;

use strict;
use warnings;

use Carp;
use Filter::Util::Call;

use Test::LectroTest::Generator qw( :common :combinators );

my $debugQ = 0;  # set to 1 to emit source-filter results on STDERR

=head1 NAME

Test::LectroTest::Property - Automated random-data tests

=head1 SYNOPSIS

 use MyModule;  # provides my_function_to_test

 use Test::LectroTest::Generator qw( :common );
 use Test::LectroTest::Property qw( Test );
 use Test::LectroTest::TestRunner;

 my $prop_non_neg = Property {
     ##[ x <- Int, y <- Int ]##
     $t->label("negative") if $x < 0;
     $t->label("odd")      if $x % 2;
     $t->retry             if $y == 0;  # 0 can't be used in test
     my_function_to_test( $x, $y ) >= 0;
 }, name => "my_function_to_test output is non-negative";

 my $runner = Test::LectroTest::TestRunner->new();
 $runner->run_suite(
     $prop_non_neg,
     # ... more properties here ...
 );

=head1 DESCRIPTION

B<STOP!>  If you're just looking for an easy way to write and run
unit tests, see Test::LectroTest first.

This module allows you to define Properties that can be tested
automatically by Test::LectroTest.  A Property defines a
behaviorial characteristic that the software you're testing
must hold over a range of inputs.

=over 4

=item 1

The shape of the haystack

=item 2

What a needle looks like

=back


=cut

BEGIN {
    use Exporter ( );
    our @ISA         = qw( Exporter );
    our @EXPORT      = qw( &Property );
    our @EXPORT_OK   = qw( &Property );
    our %EXPORT_TAGS = ( );
}
our @EXPORT_OK;
our @CARP_NOT = qw ( Test::LectroTest::TestRunner );


my %defaults = ( name => 'Unnamed Test::LectroTest::Property' );

sub all(&@) {
    my $f = shift;
    $_ || return 0 for map {$f->($_)} @_;
    1;
}

sub new {
    my $self   = shift;
    my $class  = ref($self) || $self;
    my $inputs = shift;
    if ($inputs eq "inputs") { $inputs = shift; }
    croak "Test::LectroTest::Property: invalid list of named parameters" if @_ % 2;
    croak "Test::LectroTest::Property: invalid input specification" if @$inputs % 2;
    return bless { %defaults, inputs => { @$inputs }, @_ }, $class;
}

sub import {
    Test::LectroTest::Property->export_to_level(
        1, grep {$_ ne "NO_FILTER"} @_ );
    return if grep { $_ eq "NO_FILTER" } @_;
    filter_add( make_code_filter() );
}

sub make_code_filter {
    my $content = "";
    sub {
        my $status = shift;
        if ( defined $status ? $status : ($status = filter_read()) ) {
            if (s| \#\# ( \[ .*?  ) \#*\s*\]\#\# |
                   binding($1)."]}".body($1) |exs) {
                # 1-line decl
            }
            elsif (s| \#\# ( \[.* ) | binding($1) |exs) {
                # opening of multi-line decl
                $content .= " $1";
            }
            elsif ($content && 
                   s| ^(.*?)\#*\s*\]\#\# |
                      binding($1)."]}".body("$content$1") |exs) {
                # close of multi-line decl
                $content = "";
            }
            elsif ($content && s/(.*)/binding($1)/es) {
                $content .= " $1";
            }
        }
        print STDERR if $debugQ;
        return $status;
    }
}

# convert bindinging operators ( <- ) into key arrows ( => )

sub binding {
    my $s = shift;
    $s =~ s| <- | => |gx;
    return $s;
}

sub body {
    my ($gen_decl_str) = @_;
    my @vars = $gen_decl_str =~ /(\w+)\s*<-/gs;
    @vars = keys %{{ map {($_,1)} @vars }};  # must be in hash-key order
    ' sub { my (' . join(',', map {"\$$_"} 't', @vars) . ') = @_;';
}

sub Property(&&@) {
    my ($genspec_fn, $test_fn, @args) = @_;
    return Test::LectroTest::Property->new(
        inputs => $genspec_fn->(),
        test   => $test_fn,
        @args
    );                            
}

1;



=head1 LECTROTEST HOME

The LectroTest home is 
L<http:E<sol>E<sol>community.moertel.comE<sol>LectroTest>.
There you will find more documentation, presentations, a wiki,
and other helpful LectroTest-related resources.  It's also the
best place to ask questions.

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
