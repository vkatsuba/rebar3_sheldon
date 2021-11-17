# rebar3_sheldon
[![Hex.pm Version][hexpm version]][hexpm]
[![Hex.pm Downloads][hexpm downloads]][hexpm]
[![Build Status][gh badge]][gh]
[![Erlang Versions][erlang version badge]][gh]

A rebar plugin for spellchecking code with [Sheldon](https://github.com/inaka/sheldon).

## Build

```sh
$ rebar3 compile
```

## Use

Add the plugin to your rebar config:

```erlang
{project_plugins, [{rebar3_sheldon, "~> 0.3.0"}]}.
```

Then just call your plugin directly in an existing application:
```sh
$ rebar3 spellcheck
===> Fetching rebar3_sheldon
===> Compiling rebar3_sheldon
===> Youre welcome. And if he has twins, we can do all kinds of neat experiments on them.:
test/test_SUITE.erl:1: The word "Speling" in string is unknown. Maybe you wanted to use "speeling" or "speiling" or ....?
test/test_SUITE.erl:2: The word "fdfdf" in string is unknown.
test/test_SUITE.erl:3: The word "Unicode" in comment is unknown. Maybe you wanted to use "uncoded"?
```

## Command line
### Arguments List
```sh
$ rebar3 -h spellcheck
Plugin for spellcheck by sheldon
Usage: rebar3 spellcheck [-f <files>] [-i <ignore>] [-r <ignore_regex>]
                         [-d <default_dictionary>]
                         [-a <additional_dictionaries>]

  -f, --files                    List of files for spellchecker
  -i, --ignore                   List of ignore files for spellchecker
  -r, --ignore_regex             Regular exemptions for ignore lines
  -d, --default_dictionary       Set default dictionary
  -a, --additional_dictionaries  List of additional dictionaries
```
### Short full example
```sh
$ rebar3 spellcheck -f 'src/*.erl, test/*erl' -i 'include/*.hrl' -r '[_@./#&+-=*]' -d 'path/to/dict.txt' -a 'path/to.txt, additional_dict_1.txt'`
```

## Config
### Description
By default, the dictionary used is the one provided by [sheldon](https://github.com/inaka/sheldon).
If need use custom list of files for spellchecking - use config option `files`.
If need ignore some files for spellchecking - use config option `ignore`.
If need ignore some lines - use `ignore_regex` for set regular exemptions for ignore lines.
If `sheldon` dictionary is not suite at all - can be replaced by config option `default_dictionary`.
If `sheldon` dictionary is suitable but should be expanded - use `additional_dictionaries` option to set own dictionaries for expand `sheldon` dictionary.

### Options
Currently supported next options of `spellcheck` configuration:
| Name                       | Type              | Description                           |
| -------------------------- | ----------------- | --------------------------------------|
| `files`                    | `[string(), ...]` | List of files for spellchecker        |
|  `ignore`                  | `[string(), ...]` | List of ignore files for spellchecker |
|  `ignore_regex`            | `string()`        | Regular exemptions for ignore lines   |
|  `default_dictionary`      | `string()`        | Set default dictionary                |
|  `additional_dictionaries` | `[string(), ...]` | List of additional dictionaries       |

### Default
```erlang
{spellcheck, [
    {files, ["include/**/*.[he]rl",
             "include/**/*.app.src",
             "src/**/*.[he]rl",
             "src/**/*.app.src",
             "test/**/*.[he]rl",
             "test/**/*.app.src",
             "{rebar,elvis,sys}.config"]}
]}.
```

### Example
```erlang
{spellcheck, [
    {files, ["src/*.erl", "src/*/*.erl", "include/*.hrl"]},
    {ignore, ["src/*_ignore.erl"]},
    {ignore_regex, "[_@./#&+-=*]"},
    {default_dictionary, "path/to/default_dictionary.txt"},
    {additional_dictionaries, ["path/to/custom_dictionary_1.txt", "path/to/custom_dictionary_2.txt"]}
]}.
```

<!-- Badges -->
[hexpm]: https://hex.pm/packages/rebar3_sheldon
[hexpm version]: https://img.shields.io/hexpm/v/rebar3_sheldon.svg?style=flat-square
[hexpm downloads]: https://img.shields.io/hexpm/dt/rebar3_sheldon.svg?style=flat-square
[gh]: https://github.com/vkatsuba/rebar3_sheldon/actions/workflows/ci.yml
[gh badge]: https://img.shields.io/github/workflow/status/vkatsuba/rebar3_sheldon/CI?style=flat-square
[erlang version badge]: https://img.shields.io/badge/erlang-23.0%20to%2024.1-blue.svg?style=flat-square
