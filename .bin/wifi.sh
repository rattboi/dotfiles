SSID=`iwconfig |& grep ESSID | cut -d'"' -f2`
STRENGTH=`iwconfig |& grep Link | cut -d'=' -f2 | cut -d' ' -f1`
STR_NUM=`echo "scale=2; $STRENGTH*100"|bc|cut -d'.' -f1`
echo "Wifi: [$SSID] | Strength: [$STR_NUM%]"
exit 0
