#!/bin/bash
rotation=`xrandr | grep "LVDS" | cut -d' ' -f4`

case $rotation in
  \(normal)
  	xrandr --output LVDS1 --rotation right
	xsetwacom set 10 Rotate cw
	~/.bin/trackpad-toggle.sh 1
	;;
  right)  
  	xrandr --output LVDS1 --rotation inverted
	xsetwacom set 10 Rotate half
	~/.bin/trackpad-toggle.sh 1
       	;;
  inverted)
  	xrandr --output LVDS1 --rotation left
	xsetwacom set 10 Rotate ccw
	~/.bin/trackpad-toggle.sh 1
	;;
  left)
  	xrandr --output LVDS1 --rotation normal
	xsetwacom set 10 Rotate none
	~/.bin/trackpad-toggle.sh 0
	;;
esac

exit 0
