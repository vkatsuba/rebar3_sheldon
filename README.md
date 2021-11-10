# rebar3_sheldon

A rebar plugin for spellchecking.

## Build

```sh
$ rebar3 compile
```

## Use

Add the plugin to your rebar config:

```erlang
{project_plugins, [{rebar3_sheldon, "~> 0.1.0"}]}.
```

Then just call your plugin directly in an existing application:
```sh
$ rebar3 spellcheck
===> Fetching rebar3_sheldon
===> Compiling rebar3_sheldon
$ ./rebar3 spellcheck
===> spellcheck detect warning emits:[#{filename => "src/application.erl",
                                    line => 2,
                                    reason =>
                                        #{bazinga => <<"Too bad Leonard">>,
                                          misspelled_words =>
                                              [#{candidates =>
                                                     ["commit","commot",
                                                      "comdt","comet","compt",
                                                      "comte","comdt","comet",
                                                      "compt","comte","comm",
                                                      "comm.","comma","comme",
                                                      "commo","commy","scomm"],
                                                 line_number => 1,
                                                 word => "Commt"}]},
                                    string => "Commt",type => string},
                                  #{filename => "test/shot_SUITE.erl",
                                    line => 1,
                                    reason =>
                                        #{bazinga =>
                                              <<"I'm exceedingly smart. I graduated college at fourteen. While my brother was getting an STD, I was getting a Ph.D. Penicillin can't take this away.">>,
                                          misspelled_words =>
                                              [#{candidates =>
                                                     ["speeling","speiling",
                                                      "spelding","spelling",
                                                      "sperling","spieling",
                                                      "apeling","pealing",
                                                      "peeling","pelting",
                                                      "perling","petling",
                                                      "sealing","seeling",
                                                      "selfing","seling",
                                                      "selling","setling",
                                                      "sapling","sipling",
                                                      "spiling","spaeing",
                                                      "spewing"],
                                                 line_number => 1,
                                                 word => "Speling"}]},
                                    string => "Speling",type => string}]
```

## Config
Example:
```erlang
{spellcheck, [
    {files, ["src/*.erl", "src/*/*.erl", "include/*.hrl"]},
    {ignore, ["src/*_ignore.erl"]},
    {options, #{dummy => option}}
]}.
```
