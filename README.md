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
===> Youre welcome. And if he has twins, we can do all kinds of neat experiments on them.:
test/test_SUITE.erl:1: string: The word "Speling" is unknown. Maybe you wanted to use "speeling" or "speiling" or ....?
test/test_SUITE.erl:2: string: The word "Commt" is unknown. Maybe you wanted to use "commit" or "commot" or "comdt" ...?
test/test_SUITE.erl:2: string: The word "fdfdf" is unknown.
test/test_SUITE.erl:3: comment: The word "Unicode" is unknown. Maybe you wanted to use "uncoded"?
```

## Config
Example:
```erlang
{spellcheck, [
    {files, ["src/*.erl", "src/*/*.erl", "include/*.hrl"]},
    {ignore, ["src/*_ignore.erl"]},
    {ignore_regex, "[_@./#&+-=*]"},
    {options, #{dummy => option}}
]}.
```
