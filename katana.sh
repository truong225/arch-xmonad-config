fn="M+ 1m-9:Bold"	# Set the main font as the panel
obg="#EEEEEE"
ofg="#333333"

conky -c "/script/statusbar.lua" | dzen2 -p -ta r -x 400 -h 22 -w 1000 -fn "$fn" -bg "$obg" -fg "$ofg"
