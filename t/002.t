# should always test OK

# use MyModule;  # provides my_function_to_test

use File::Basename;
use Test::LectroTest scalefn => sub { $_[0] / 2 + 3 }, verbose => 1, debug => 1;

my $intgen = Int;

Property {
    ##[ ]##
    basename("a/b") eq "b";
}, name => "0-arg always succeeds" ;

Property {
    ##[ #]##
    basename("a/b") eq "b";
}, name => "0-arg, alt-syntax always succeeds" ;

Property {
    ##[ x <- $intgen ]##
    $t->label("negative") if $x < 0;
    $t->label("odd")      if $x % 2;
    1;
}, name => "1-arg always succeeds (labels, too)" ;

Property {
    ##[ 
        x <- $intgen
    #]##
    1;
}, name => "1-arg, alt-syntax always succeeds" ;

Property {
    ##[ 
        x <- $intgen
    # ]##
    1;
}, name => "1-arg, alt2-syntax always succeeds" ;

Property {
    ##[ 
        x <- $intgen
    ####]##
    1;
}, name => "1-arg, alt3-syntax always succeeds" ;

Property {
    ##[ 
        x <- $intgen
    #### ]##
    1;
}, name => "1-arg, alt4-syntax always succeeds" ;

Property {
    ##[ x <- Float, y <- Int ]##
    1;
}, name => "2-arg always succeeds" ;

Property {
    ##[ x <- Unit(1), a <- Unit(2), c <- Unit(3), y <-Unit(4) ]##
    $x == 1 && $a == 2 && $c == 3 && $y == 4;
}, name => "argument order is preserved"

