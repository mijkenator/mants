#!/bin/bash

#pwd

if [[ $1 == "clear" ]]; then
    echo "Clearing ... ";
    find ./deps/ -name *.beam -exec rm -f {} +
    find ./apps/ -name *.beam -exec rm -f {} +
fi;

#. /opt/r15b02/activate
#kerl_deactivate

PATH="/usr/local/bin:/usr/bin:$PATH" ./rebar compile && PATH="/usr/local/bin:/usr/bin:$PATH" ./rebar -f generate
