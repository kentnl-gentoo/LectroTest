#!/usr/bin/perl

use warnings;
use strict;

use Test::LectroTest::Property;
use Test::LectroTest::TestRunner;
use Test::More tests => 3;

=head1 NAME

t/003.t - Error-handling tests

=head1 SYNOPSIS

    perl -Ilib t/003.t

=head1 DESCRIPTION

First, we see whether LectroTest::Property prevents you from
using the reserved identifier "tcon" in a generator-binding
declaration.

=cut

eval { 
    Property { [ tcon => 1 ] } sub {
        1;
    }
};
like($@, qr/cannot use reserved name 'tcon' in a generator binding/,
   "Property->new disallows use of 'tcon' in bindings");

eval { 
    Property { ##[ tcon <- 1 ]##
        1;
    }
};
like($@, qr/cannot use reserved name 'tcon' in a generator binding/,
   "magic Property syntax disallows use of 'tcon' in bindings");


=pod

Second, we see whether exceptions throw (e.g., via die) during
testing are caught and reported.

=cut

my $will_throw = Property {
    ##[ ]##
    die "test threw exception";
};

sub run_details($);
like( run_details($will_throw), qr/test threw exception/, 
      "exceptions are caught and reported as failures" );


=head1 HELPERS

The C<run_details> helper runs a check of the given
property and returns the C<details> of the results.

=cut

sub run_details($) {
    my $runner = new Test::LectroTest::TestRunner;
    return $runner->run(@_)->details;
}



=head1 AUTHOR

Tom Moertel (tom@moertel.com)

=head1 COPYRIGHT and LICENSE

Copyright (C) 2004 by Thomas G Moertel.  All rights reserved.

This program is free software; you can redistribute it and/or
modify it under the same terms as Perl itself.
