#!/usr/bin/env zsh

ERL=erl
ROOT=/Users/noh/Downloads/test/webapi
API_EBIN=$ROOT/mod_router/ebin
LIB_EBIN=$ROOT/lib/jiffy/ebin

PNAME=webapi

start() {
	 $ERL -detached -pa $API_EBIN -pa $LIB_EBIN -config $ROOT/sys \
	      -service_name $PNAME -s inets start -s router_app start
}

stop() {
	while IFS= read -r pid
	do
        kill -9 $pid
	done < <(ps auxww | grep "\-service_name $PNAME" | grep -v grep | awk '{print $2}')
}

status() {
    while IFS= read -r pid
    do
        echo $pid stared...
    done < <(ps auxww | grep "\-service_name $PNAME" | grep -v grep | awk '{print $2}')
}

case "$1" in
start)
    echo "=> starting..."
    stop
    sleep 2
    start
    ;;
stop)
    stop
    ;;
restart)
    stop 
    sleep 2
    start
    ;;
status)
    status
    ;;
*)
    echo "=> starting..."
    stop
    sleep 2
    start
esac
