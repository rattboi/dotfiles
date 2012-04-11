bars=$(amixer get Master | awk -F'[]%[]' '/%/ {if ($7 == "off") { print "MM" } else { print int($2/10)}}' | head -n 1)

case $bars in
  0)  bar='[----------]' ;;
  1)  bar='[#---------]' ;;
  2)  bar='[##--------]' ;;
  3)  bar='[###-------]' ;;
  4)  bar='[####------]' ;;
  5)  bar='[#####-----]' ;;
  6)  bar='[######----]' ;;
  7)  bar='[#######---]' ;;
  8)  bar='[########--]' ;;
  9)  bar='[#########-]' ;;
  10) bar='[##########]' ;;
  MM) bar='[M--U--T--E]' ;;
esac

echo Vol: $bar

exit 0
