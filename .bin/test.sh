#!/bin/bash
old="0"
while true; do
	new=$(cat /sys/devices/platform/hp-wmi/tablet)
	if [ "$new" != "$old" ]; then
		if [ $new == "1" ]; then
			~/.bin/rotate.sh
			~/.bin/rotate.sh
			~/.bin/rotate.sh
		else
			~/.bin/rotate.sh
		fi
		old=$new
	fi
	sleep 1s
done
