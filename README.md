# Solar API

We got a new solar panel setup with monitoring and a smart meter.
Sounds like a project.

## Setup

It's set up to use nix flakes.

You will have to get `GoSungrow` going ahead of time with

    nix run ./go-sungrow#go-sungrow -- config write --user=<user> --password=<password>

I don't know how to do this kind of secrets-management with nix, let
alone do it for some special-snowflake Go program.
