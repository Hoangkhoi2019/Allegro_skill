;######################################################
axlCmdRegister("spread_cline" `spread_cline )
(defun spread_cline ()
	gridSnap1 = axlEnterPoint(?prompts list("Pick origin point") ?gridSnap t)
	gridSnap2 = axlEnterPoint(?prompts list("Pick origin point") ?gridSnap t)
	if(gridSnap1 != nil && gridSnap2 != nil then 
		a = axlMakeDynamicsPath(list(list( gridSnap1 gridSnap2 0.0)))
		poly = car(axlPolyFromDB(a ?line2poly t))
		axlClearSelSet()
		axlSetFindFilter(?enabled '(noall CLINESEGS NAMEFORM) ?onButtons '(noall CLINESEGS))
		axlSingleSelectBox(poly->bBox)
		seg_list = axlGetSelSet()
		f_seg_list = nil
		foreach(seg seg_list
			tmp = axl_ol_ol2(list(gridSnap1 gridSnap2) seg->startEnd)
			if(caddr(tmp) == 0 then
				chk = list( axlDistance(car(tmp) gridSnap1) axlDistance(car(tmp) gridSnap2))
				if(car(setof(e chk e < seg->width)) == nil then
					f_seg_list = cons(list(car(tmp) seg->net->diffpair) f_seg_list)
					axlHighlightObject(seg->parent)
				)
				
			)
		)
		
		if(length(f_seg_list)==2 && cadr(car(f_seg_list)) != nil then
			p_item1 = caar(f_seg_list)
			p_item2 = car(cadr(f_seg_list))
			mid_point = axlMidPointLine(list(p_item1 p_item2))
			mid_point2 = axlMidPointLine(list(gridSnap1 gridSnap2))
			cus_shell = sprintf(nil "slide ; prepopup %f %f ; group ; pick grid %f %f ; pick grid %f %f; prepopup %f %f ;complete ; pick grid %f %f ;pick grid %f %f" car(p_item1) cadr(p_item1) car(p_item1) cadr(p_item1) car(p_item2) cadr(p_item2) car(mid_point) cadr(mid_point) car(mid_point) cadr(mid_point) car(mid_point2) cadr(mid_point2))	
			axlShellPost(cus_shell)
			axlShellPost("done")
			axlShellPost("smooth_seg")
		else
			point_list = nil
			dis_ori = axlDistance(gridSnap1 gridSnap2)
			;CreatePath = axlDBCreateLine( list(gridSnap1 gridSnap2), 2.0, "ANTI ETCH/ALL" )
			val_tmp = dis_ori/ (length(f_seg_list) +1)
			loop = val_tmp
			while(loop<dis_ori
				div_val = loop
				tmp_line = list(gridSnap1 axlMXYAdd(gridSnap1 0 : div_val ))
				angle = axlRadToDeg(_axlAngleBetweenLines(tmp_line , list(gridSnap1 gridSnap2)))
				tmp_point = axlMXYAdd(gridSnap1 0 : div_val )
				tmp_point = axlGeoRotatePt(angle ,tmp_point, gridSnap1)
				angle2 = abs(axlRadToDeg(_axlAngleBetweenLines(list(gridSnap1 tmp_point) , list(gridSnap1 gridSnap2))))
				tmp2= list( axlDistance(tmp_point gridSnap1) axlDistance(tmp_point gridSnap2))
				if(car(setof(e tmp2 e > dis_ori)) != nil then
					tmp_point = axlGeoRotatePt(180.0 ,tmp_point, gridSnap1)
				)
				;CreatePath = axlDBCreateLine( list(gridSnap1 tmp_point), 2.0, "ANTI ETCH/TOP" )
				point_list = cons(tmp_point point_list)
				loop = loop+val_tmp
			)
			foreach(point point_list
				temp = nil
				foreach(item f_seg_list
					temp = cons(list(axlDistance(car(item) point) car(item) point lastelem(item)) temp)
				)
				temp2 = car(sortcar(temp 'lessp))
				p1 = cadr(temp2)
				p2 = caddr(temp2)
				
				
				if(lastelem(temp2) != nil then
					cus_shell = sprintf(nil "slide; pick grid %f %f ; pop singletrace off ;pick grid %f %f" car(p1) cadr(p1) car(p2) cadr(p2))
				else
					cus_shell = sprintf(nil "slide; pick grid %f %f ;pick grid %f %f" car(p1) cadr(p1) car(p2) cadr(p2))
				)
				axlShellPost(cus_shell)
			)
			axlShellPost("done")
			axlShellPost("smooth_seg")
		)
	)
	
	
	
)
;######################################################
