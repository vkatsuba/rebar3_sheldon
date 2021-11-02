# rebar3_sheldon

A rebar plugin description

## Build

```sh
$ rebar3 compile
```

## Use

Add the plugin to your rebar config:

```erlang
{project_plugins, [{rebar3_sheldon, "~> 0.0.0"}]}.
```

Then just call your plugin directly in an existing application:
```sh
$ rebar3 rebar3_sheldon
===> Fetching rebar3_sheldon
===> Compiling rebar3_sheldon
<Plugin Output>
```
