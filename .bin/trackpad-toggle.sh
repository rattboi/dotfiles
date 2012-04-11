#!/bin/bash

num=$#
case $# in 
  0)
	synclient TouchpadOff=$(synclient -l | grep -c 'TouchpadOff.*=.*0')
	;;
  *)	synclient TouchpadOff=$1
	  ;;
esac

