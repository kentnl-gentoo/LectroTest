package Test::LectroTest;

use warnings;
use strict;

use Test::LectroTest::TestRunner;
use Filter::Util::Call;
require Test::LectroTest::Property;
require Test::LectroTest::Generator;

our $VERSION = .20_02;

=head1 NAME 

Test::LectroTest - Easy, automatic, specification-based tests

=head1 SYNOPSIS

    #!/usr/bin/perl -w

    use MyModule;  # contains code we want to test
    use Test::LectroTest;

    Property {
        ##[ x <- Int, y <- Int ]##
        MyModule::my_function( $x, $y ) >= 0;
    }, name => "my_function output is non-negative" ;

    Property { ... }, name => "yet another property" ;

    # more properties to check here

=head1 DESCRIPTION

This module provides a simple (yet full featured) interface to
LectroTest, automated, specification-based testing system for Perl.
To use it, you declare properties that specify the expected behavior
of your software.  LectroTest then checks to see whether those
properties hold.

You declare properties using the Property function, which takes a
block and promotes it to a LectroTest::Property:

    Property {
        ##[ x <- Int, y <- Int ]##
        MyModule::my_function( $x, $y ) >= 0;
    }, name => "my_function output is non-negative" ;

The first part of the block must contain a generator-binding
declaration.  For example:

        ##[  x <- Int, y <- Int  ]##

(Note the special bracketing, which must be present.)  This particular
binding says, "For all integers I<x> and I<y>."  (See
LectroTest::Generator to see the whopping passel of generators that
are at your disposal.)

The second part of the block is just a bit of code that makes use
of the variables we bound earlier to test whether a property holds
for the piece of software we are testing:

        MyModule::my_function( $x, $y ) >= 0;

In this case, it asserts that MyModule::my_function($x,$y) returns a
non-negative result.  (Yes, C<$x> and C<$y> refer to the same I<x> and
I<y> that we bound to the generators earlier.  LectroTest
automagically loads these Perl variables with values behind the
scenes.)

Finally, we give the whole Property a name, in this case "my_function
output is non-negative."  It's a good idea to use a meaningful name
because LectroTest refers to properties by name in its output.

Let's take a look at the finished property:

    Property {
        ##[ x <- Int, y <- Int ]##
        MyModule::my_function( $x, $y ) >= 0;
    }, name => "my_function output is non-negative" ;

It says, "For all integers I<x> and I<y>, we assert that my_function's
output is non-negative."

To check whether this property holds, simply put it in a Perl program
that uses the Test::LectroTest module.  (See the L</SYNOPSIS> for an
example.)  When you run the program, LectroTest will load the property
(and any others in the file) and check it by running random trials
against the software.  If LectroTest "breaks" your software, it will
emit a counterexample and stop.  You can plug the counterexample back
into your software to debug the problem.  (You might also want to add
the counterexample to a list of regression tests.)

A successful LectroTest looks like this:

  1..1
  ok 1 - 'my_function output is non-negative' (1000 attempts)

On the other hand, if you're not so lucky:

  1..1
  not ok 1 - 'my_function output is non-negative' falsified \
      in 324 attempts
  # Counterexample:
  # $x = -34
  # $y = 0

=head1 ADJUSTING THE TESTING PARAMETERS

There is one testing parameter that you may wish to change: The number
of trials to run against each property checked.  By default it is
1,000.  If you want to try more or fewer trials, pass the
C<trials=E<gt>>I<N> flag:

  use Test::LectroTest trials => 10_000;


=head1 SEE ALSO

For a more in-depth introduction to LectroTest, see
LectroTest::Tutorial.  For more information on the various parts of
LectroTest, see LectroTest::Property, LectroTest::Generator, and
LectroTest::TestRunner.

Also, the slides from my LectroTest talk for the Pittsburgh Perl
Mongers is a great introduction.  Download a copy from the LectroTest
home (see below).


=cut

our $r;
our @ts;
our @opts;

sub import {
    my $self = shift;
    Test::LectroTest::Property->export_to_level(1, $self);
    Test::LectroTest::Generator->export_to_level(
        1, $self, qw(:common :combinators Gen) );
    @opts = @_;
    $r = Test::LectroTest::TestRunner->new( @_ );
    my $lines = 0;
    my $subfilter = Test::LectroTest::Property::make_code_filter();
    filter_add( sub {
        my $status = filter_read();
        $_ .= 'END{Test::LectroTest::run()} ' unless $lines++;
        s{^(?=Test|Property)\b}{push \@Test::LectroTest::ts, };
        $subfilter->( $status );
    });
}

sub run {
    $r->run_suite( @ts, @opts );
}

1;

__END__

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
