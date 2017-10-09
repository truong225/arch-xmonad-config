conky.config={
	background=false,
	out_to_console = true,
	out_to_x = false,
	update_interval = 1,
	total_run_times = 0,
	use_spacer = 'none'
}

arrowUp='^i(~/.xmonad/xbm/.xmonad/dzen2/xbm8x8/uparrow1.xbm)'
timeIcon='^bg(' .. arrowUp .. ')'

conky.text=[[\
	${time %a, %b-%d %H:%M:%S}	\
	${color grey}RAM:$color $mem	\
	${color grey}CPU:$color ${cpu cpu0}%	\
]]


-- ${color darkorange}HDD:$fs_free | $fs_size	\