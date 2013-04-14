#! /bin/sh

erl -pa apps/*/ebin -boot start_sasl -s biten_app
