use Module::Build;
my $build = Module::Build->new
( module_name        => "Test::LectroTest",
  license            => "perl",
  create_makefile_pl => "traditional",
  create_readme      => 1,
  requires           => { Test::More => 0, Filter::Util::Call => 0 },
);

$build->create_build_script;
