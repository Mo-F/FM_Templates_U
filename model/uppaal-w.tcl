<'
# Copyright (c) 2012-2015 LAAS/CNRS
# All rights reserved.
#
# Redistribution and use  in source  and binary  forms,  with or without
# modification, are permitted provided that the following conditions are
# met:
#
#   1. Redistributions of  source  code must retain the  above copyright
#      notice and this list of conditions.
#   2. Redistributions in binary form must reproduce the above copyright
#      notice and  this list of  conditions in the  documentation and/or
#      other materials provided with the distribution.
#
#                                      Felix Ingrand on July 20 2012
#


#if {[llength $argv] != 1} { error "expected arguments: component" }
#lassign $argv component

#set types [$component types public]

# compute handy shortcuts
#set comp [$component name]
#set COMP [string toupper [$component name]]

#lang c
'>

<'#if {[llength $argv] != 1} { error "expected arguments: component" }
#lassign $argv component

#set types [$component types public]

# compute handy shortcuts
#set comp [$component name]
#set COMP [string toupper [$component name]]

lang c
'>


/* This UPPAAL sepcification is automatically generated. Mohammed Foughali (LAAS/MDH, May 2017) */

<' set w [list] 
foreach co [dotgen components] {
	foreach se [$co services] {
		if {[$se name]!="abort_activity" && [$se name]!="kill" && [$se name]!="connect_port" && 
		[$se name]!="connect_service"} {
		foreach c [$se codels] {
		lappend w [[$c wcet] value]}}}
	foreach ta [$co tasks] {
		foreach c [$ta codels] {
		foreach tr [$c triggers] {
		if {[$tr name] != "stop"} {
		lappend w [[$c wcet] value]
		break}
}
}
}
}'>

<' set w [lsort -real $w]
set min [lindex $w 0]
set coef 0
while {$min < 1} {
	set min [expr $min*10]
	incr coef}
set coef [expr pow(10,$coef)]'>

<'if {[llength [dotgen components]] > 1} {
set np [ports-number dotgen]'>

/* ports */

const int nports:= <"$np">;
bool ports[nports]:= { false<'
for {set k 2} {$k <= $np} {incr k} {'>, false<'}'>};

<'}'>

/* urgency */

urgent chan exe;

<'
set compos [list]
set index 0	
foreach comp [dotgen components] {
set cn [$comp name]
set services($index) [list]
set needsvalidate($index) [list]
set confcod($index) 0
set activities($index) [list]
set nonactivities($index) [list]
foreach t [$comp tasks] {
	foreach c [$t codels] {
		if {[llength [$c mutex]] } {
		incr confcod($index)}} 
	foreach s [$t services] {
		foreach c [$s validate] {
		if {[llength [$c mutex]] } {
		incr confcod($index)}
		lappend needsvalidate($index) $s}
		foreach c [$s codels] {
		if {[llength [$c mutex]] } {
		incr confcod($index)}}
	lappend services($index) $s
	lappend activities($index) $s}}
foreach s [$comp services] {
	if {[$s name]!="abort_activity" && [$s name]!="kill" && [$s name]!="connect_port" && 
		[$s name]!="connect_service" && [$s kind]!="activity"} {
		lappend services($index) $s
		lappend nonactivities($index) $s
		if {[llength [$s mutex]] } {
		incr confcod($index)
		} else {
		foreach c [$s codels] {
		if {[llength [$c mutex]] } {
		 incr confcod($index)
		 break}

}}}}'>
/* channels, variables and functions (<"$cn">) */
/* channels */

urgent chan recv_urg_<"$cn"><'
foreach t [$comp tasks] {
	if {![catch {$t period}]} {'>, shuttimer_<"[$t name]">_<"$cn"><'}
	if {[llength [$t services]]} {'>, begin_<"[$t name]">_<"$cn">, inter_<"[$t name]">_<"$cn"><'}}'>;
chan recv_<"$cn">;<'

set lastperm($index) 0
for {set k [expr [llength [$comp tasks]]-1]} {$k >= 0} {incr k -1} {
	if {[llength [[lindex [$comp tasks] $k] codels]]} {set lastperm($index) $k
		break}}
set go($index) [list]
set stops($index) [list]
if {$lastperm($index) > -1} {'>
	
broadcast chan end_spawn_<"[[lindex [$comp tasks] $lastperm($index)] name]">_<"$cn"><'
foreach t [$comp tasks] {
	if {[llength [$t codels]]} {
		if  {$t != [lindex [$comp tasks] $lastperm($index)]} {'>, end_spawn_<"[$t name]">_<"$cn"><'}
		set test 0
		foreach c [$t codels] {
			foreach tr [$c triggers] {
				if {[$tr name] == "stop"} {
					lappend stops($index) $t
					break}}
			foreach y [$c yields] {
				if {[$y kind] == "pause event"} {
					set test 1
					break}}
				if {$test} { lappend go($index) $t 
					set test 0
					break}}
		}}'>;<'
if {[llength $go($index)]} {'>
	
urgent broadcast chan go_<"[[lindex $go($index) 0] name]">_<"$cn"><'
set go2 [lreplace $go($index) 0 0]
foreach g $go2 {'>, go_<"[$g name]">_<"$cn"><'}'>;<'}
	
}'>


/* constants & global variables */
bool shut_<"$cn">:= false;
<'if {[llength $activities($index)]} {'>

bool sched_<"$cn">:= false;<'}

foreach t [$comp tasks] {'>

bool lock_<"[$t name]">_<"$cn">:= true;<'
if [llength [$t services]] {'>

bool finished_<"[$t name]">_<"$cn">:= false;<'}
if {![catch {$t period}]} {'>

bool tick_<"[$t name]">_<"$cn">:= false;<'}
}'>

/* services IDs */
/* attributes, activities & functions */
<'set id 0
foreach s $services($index) {'>
const int <"[$s name]">_<"$cn">:= <"$id">;<'
incr id}'>

const int nserv_<"$cn">:= <"[llength $services($index)]">;
<'if {![llength $activities($index)]} {'>
const int kill_<"$cn">:=-1;

<'} else {'>
const int width_<"$cn">:= <"[llength $activities($index)]">;
const int size_<"$cn">:= width_<"$cn">*2+1;
// the abort request is a negative "i" number that refers to interrupting the element whose index is i+size-1 
const int kill_<"$cn">:= -1*size_<"$cn">;

/* services with validate */

<'if {[llength $needsvalidate($index)]} {'>

const int numval_<"$cn"> := <"[llength $needsvalidate($index)]">;
const int val_<"$cn">[numval_<"$cn">] := {<'
foreach n $needsvalidate($index) {'><"[$n name]">_<"$cn"><'
	if {$n != [lindex $needsvalidate($index) end]} {'>, <'}}'>};<'}'>

/* none (urgent reaction of CT with no request in the mbox) */
const int none_<"$cn">:= kill_<"$cn"> -1;

/* status */
// this choice is deleberate to ease manipulating interruptions
const int RUN_<"$cn">:= 0;
const int WAIT_<"$cn">:= 1;
const int STOP_<"$cn">:= 2;
const int ETHER_<"$cn">:= 3; 
const int VOID_<"$cn">:= 4;

/* activities array */
/* structure */ 
typedef struct {int [-1,width_<"$cn">-1] name; int [1,2] inst; int [RUN_<"$cn">,VOID_<"$cn">] status;} cell_<"$cn">;
/* initialisation */
cell_<"$cn"> tab_<"$cn">[size_<"$cn">] := { <'
	foreach a $activities($index) {'>{<"[$a name]">_<"$cn">,1,VOID_<"$cn">}, {<"[$a name]">_<"$cn">,2,VOID_<"$cn">}, <'}'>{-1,1,VOID_<"$cn">} }; <'
}'>

int [<'
	if {[llength $activities($index)]} {'>none<'
		} else {'>kill<'}'>_<"$cn">,nserv_<"$cn">-1] req_<"$cn">:= kill_<"$cn">;

<'foreach t [$comp tasks] {
if [llength [$t services]] {'>

cell_<"$cn"> turn_<"[$t name]">_<"$cn">:={-1,1,VOID_<"$cn">};<'}}'>



<'if {$confcod($index)} {'>

/* mutual exclusion array */

const int length_mut_<"$cn">:= <"$confcod($index)">;
bool mutex_<"$cn">[length_mut_<"$cn">]:= {<'
for {set k 1} {$k <= $confcod($index)} {incr k} {'>false<'
	if {$k < $confcod($index)} {'>, <'}}'>};<'}'>

/* functions */

<' if {[llength $activities($index)]} {
if {[llength $needsvalidate($index)]} {'>
	
/* may go to manage (no validate) */

bool noval_<"$cn">(int[<"[[lindex $activities($index) 0] name]">_<"$cn">, <"[[lindex $activities($index) end] name]">_<"$cn">] req) {
int[0,numval_<"$cn">] i;
for (i:=0;i<numval_<"$cn">;i++) {
	if (req==val_<"$cn">[i]) {return false;}}
return true;}<'}'>

/* abort and kill effect */
	
	
void interrupt_cntrl_<"$cn"> (int[none_<"$cn">,-1] i, cell_<"$cn"> &tab[size_<"$cn">], bool &shut) {
int[0,size_<"$cn">-1] j;
if (i==none_<"$cn">) {return;}
if (i!=kill_<"$cn">) {
            if (tab[i+size_<"$cn">-1].status<STOP_<"$cn">) {tab[i+size_<"$cn">-1].status += STOP_<"$cn">;}
						return;}
for (j:=0; j<size_<"$cn">-1; j++) {
                    if (tab[j].status<STOP_<"$cn">) {tab[j].status += STOP_<"$cn">;}}
shut := true;
}

/* clear and launch */
void launch_serv_<"$cn"> (cell_<"$cn"> &tab[size_<"$cn">]) {
int i,j;
bool launch:= true;
for (i:=0; i<size_<"$cn">-1; i++) {
            if (tab[i].status==ETHER_<"$cn">) {tab[i].status:=VOID_<"$cn">;}//final replies
}
for (i:=0; i<size_<"$cn">-1; i++) {
    if (tab[i].status==WAIT_<"$cn">) {<'
		set nointer($index) [list]
		set inter($index) [list]
		foreach a $activities($index) {
			if {[llength [$a interrupts]]} {lappend inter($index) $a
				} else {lappend nointer($index) $a}}
		if {[llength $nointer($index)]} {'>
		
        if (<'
        foreach n $nointer($index) {'>tab[i].name==<"[$n name]">_<"$cn"><'
			if {$n != [lindex $nointer($index) end]} {'> || <'}}'>) {tab[i].status:=RUN_<"$cn">;}
        else {
<'}
foreach i $inter($index) {
	if {$i != [lindex $inter($index) 0]} {'>
	
   else<'}'>

   if (tab[i].name==<"[$i name]">_<"$cn">) {
   for (j:= 0; (j<size_<"$cn">-1 && launch); j++) {
                if (j!=i && (<'
                foreach in [$i interrupts] {'>tab[j].name == <"[$in name]">_<"$cn"><'
				if {$in != [lindex [$i interrupts] end]} {'> || <'}}'>) && tab[j].status!=VOID_<"$cn">) {
                    launch:= false;}}
               if (launch) {tab[i].status:=RUN_<"$cn">;}
               else {launch:= true;}}
       <'}'>
}
}
}
<'if {[llength $nointer($index)]} {'>}<'}'>
                    
/* interrupt incompatible instances */
void manage_intterupt_<"$cn"> (int[0,nserv_<"$cn">-1] s, int[0,size_<"$cn">-2] i, cell_<"$cn"> &tab[size_<"$cn">]) {
int[0,size_<"$cn">-1] j;
<'foreach n $nonactivities($index) {
	if {[llength [$n interrupts]]} {'>
	
	if (s == <"[$n name]">_<"$cn">) {
		for (j:= 0; j<size_<"$cn">-1; j++) {
		if ((<'
		foreach in [$n interrupts] {'>tab[j].name == <"[$in name]">_<"$cn"><'
				if {$in != [lindex [$n interrupts] end]} {'> || <'}}'>) && tab[j].status<2) {tab[j].status += 2;}}
				return;}<'}}
				
foreach i $inter($index) {'>
	
	if (s == <"[$i name]">_<"$cn">) {
		for (j:= 0; j<size_<"$cn">-1; j++) {
		if (j != i && (<'
		foreach in [$i interrupts] {'>tab[j].name == <"[$in name]">_<"$cn"><'
				if {$in != [lindex [$i interrupts] end]} {'> || <'}}'>) && tab[j].status<2) {tab[j].status += 2;}}
				return;}<'}'>

}

/* update status of terminated services */
void update_<"$cn"> (bool &finished, int[0,size_<"$cn">-2] i, cell_<"$cn"> &tab[size_<"$cn">]) {
if (finished) {finished:= false;
                tab[i].status:= ETHER_<"$cn">;}
}

/* signal the end of at least an activity */
bool sched_cntrl_<"$cn"> (cell_<"$cn"> tab[size_<"$cn">]) {
int[0,size_<"$cn">-1] i;
for (i:=0; i<size_<"$cn">-1; i++) {
                       if (tab[i].status == ETHER_<"$cn">) {return true;}}
return false;}

/* look for a slot and interrupt if necesary */
void slot_search_<"$cn"> (int[0,width_<"$cn">-1] a, cell_<"$cn"> &tab[size_<"$cn">]) {
int[0,size_<"$cn">-1] i:= 0;
while (tab[i].name != a) {i:= i+1;}
while (tab[i].name == a) {if (tab[i].status == VOID_<"$cn">)
                           {tab[i].status := WAIT_<"$cn">;
                            manage_intterupt_<"$cn">(a, i, tab);
                            return;}
                            i:= i+1;}
return;}

/* look for the next instance to execute */
int next_<"$cn"> (int[0,size_<"$cn">-1] i, cell_<"$cn"> tab[size_<"$cn">], int[0,size_<"$cn">-1] stop_point) {
while (i < stop_point) {
 if (tab[i].status == RUN_<"$cn"> || tab[i].status == STOP_<"$cn">) {return i;}
 i:= i+1;}
return size_<"$cn">-1;}

/* shutdown */
bool off_signal_<"$cn"> (cell_<"$cn"> tab[size_<"$cn">]) {
int[0,size_<"$cn">-1] j;
for (j:= 0; j<size_<"$cn">-1; j++) {
    if (tab[j].status != VOID_<"$cn"> && tab[j].status != ETHER_<"$cn">) {return false;}}
return true;}

<'}

if {[llength $go($index)]} {'>
	
/* determine whether it is the first execution of a permanent activity */
void update_lock_<"$cn"> (bool shift, bool &lock) {
if (!shift) {lock:= true;}
}
<'}
incr index}'>	
	
<'set index 0
foreach comp [dotgen components] {
set cn [$comp name]'>

/* processes (<"$cn">)*/

/* control task */

process control_<"$cn">(chan &recv_<"$cn">, urgent chan &exe, bool &shut_<"$cn"><'
if {[llength $activities($index)]} {'>, urgent chan &recv_urg_<"$cn">, cell_<"$cn"> &tab_<"$cn">[size_<"$cn">], bool &sched_<"$cn"><'}
if {$lastperm($index) > -1} {'>, broadcast chan &end_spawn_<"[[lindex [$comp tasks] $lastperm($index)] name]">_<"$cn"><'}'>, int[<'
if {[llength $activities($index)]} {'>none<'
		} else {'>kill<'}'>_<"$cn">,nserv_<"$cn">-1] &req_<"$cn"><'
if {[llength $services($index)] && $confcod($index)} {'>, bool &mutex_<"$cn">[length_mut_<"$cn">]<'}'>) {
clock x;
state shutdown, receive<'
if {$lastperm($index) > -1} {'>, unspawned<'}
if {[llength $services($index)]} {'>, decode<'
	if {[llength $activities($index)]} {'>, manage, wait, finish<'
		foreach a $activities($index) {
			foreach v [$a validate] {'>, val_<"[$a name]"><'
				if {[llength [$v mutex]]} {'>, val_<"[$a name]">_2<'}'> {x<=<"[expr round(ceil([[$v wcet] value]*$coef))]">}<'}}}
		foreach n $nonactivities($index) {'>, <"[$n name]">_<'
			set conflicts [$n mutex]
			set wcet 0
			foreach c [$n codels] {lappend conflicts [$c mutex]
				set wcet [expr round(ceil([[$c wcet] value]*$coef))]}
			if {[llength $conflicts]} {'>, <"[$n name]">_2<'}'> {x<=<"$wcet">}<'
			foreach v [$n validate] {'>, val_<"[$n name]"><'
				if {[llength [$v mutex]]} {'>, val_<"[$n name]">_2<'}'>{x<=<"[expr round(ceil([[$v wcet] value]*$coef))]">}<'}}'>;
commit decode;
urgent finish<'
if {[llength $activities($index)]} {'>, manage<'}}'>;

init <'
if {$lastperm($index) > -1} {'>unspawned<'
	} else {'>receive<'}'>;

trans <'if {$lastperm($index) > -1} {'>
	unspawned -> receive {sync end_spawn_<"[[lindex [$comp tasks] $lastperm($index)] name]">_<"$cn">?; },
	<'}
	if {[llength $activities($index)]} {'>
receive -> decode { guard sched_<"$cn">; sync recv_urg_<"$cn">?; assign sched_<"$cn">:= false; },
	decode -> finish { guard req_<"$cn"> < 0; assign interrupt_cntrl_<"$cn">(req_<"$cn">,tab_<"$cn">,shut_<"$cn">);  },
	manage -> finish { assign slot_search_<"$cn"> (req_<"$cn">, tab_<"$cn">);  },
	decode -> manage { guard req_<"$cn"> >=<"[[lindex $activities($index) 0] name]">_<"$cn">  && req_<"$cn"> <=<"[[lindex $activities($index) end] name]">_<"$cn"><'
if {[llength $needsvalidate($index)]} {'> && noval_<"$cn">(req_<"$cn">)<'}'>;  },
	finish -> receive { guard !shut_<"$cn">; assign launch_serv_<"$cn">(tab_<"$cn">);  },
	receive -> decode { guard !sched_<"$cn">; sync recv_<"$cn">?;  },<'
foreach a $activities($index) {
	if {$a in $needsvalidate($index)} {'>

	decode -> val_<"[$a name]"> <'
	foreach v [$a validate] {
		if [llength [$v mutex]] {'>{ guard req_<"$cn">==<"[$a name]">_<"$cn">; },
	val_<"[$a name]"> -> val_<"[$a name]">_2 {guard <'
	set mutex [mutex-indexes $comp $v]
foreach m $mutex {
	if {$m != [lindex $mutex end]} {'>!mutex_<"$cn">[<"[lindex $m 0]">]/*in conflict with <"[lindex $m 1]">_<"[lindex $m 2]">*/<'
	if {$m != [lindex $mutex end-1]} {'> && <'}}}'>; sync exe!; assign x:=0, mutex_<"$cn">[<"$m">]:= true; },
	val_<"[$a name]">_2 -> manage {guard x==<"[expr round(ceil([[$v wcet] value]*$coef))]">; assign mutex_<"$cn">[<"$m">]:= false;},
	val_<"[$a name]">_2 -> finish {guard x==<"[expr round(ceil([[$v wcet] value]*$coef))]">; assign mutex_<"$cn">[<"$m">]:= false;},<'
} else {'>{ guard req_<"$cn">==<"[$a name]">_<"$cn">; assign x:= 0; },
	val_<"[$a name]"> -> manage {guard x==<"[expr round(ceil([[$v wcet] value]*$coef))]">; },
	val_<"[$a name]"> -> finish {guard x==<"[expr round(ceil([[$v wcet] value]*$coef))]">; },<'}}}}'>
	


<'} 

foreach n $nonactivities($index) {
	set conflicts [$n mutex]
	foreach c [$n codels] {
		foreach m [$c mutex] {
			lappend conflicts $m}}
	if {[llength [$n validate]]} {'>
	decode -> val_<"[$n name]"> <'
	foreach v [$n validate] {
		if [llength [$v mutex]] {'>{ guard req_<"$cn">==<"[$n name]">_<"$cn">; },
	val_<"[$n name]"> -> val_<"[$n name]">_2 {guard <'
	set mutex [mutex-indexes $comp $v]
foreach m $mutex {
	if {$m != [lindex $mutex end]} {'>!mutex_<"$cn">[<"[lindex $m 0]">]/*in conflict with <"[lindex $m 1]">_<"[lindex $m 2]">*/<'
	if {$m != [lindex $mutex end-1]} {'> && <'}}}'>; sync exe!; assign x:=0, mutex_<"$cn">[<"$m">]:= true; },
	val_<"[$n name]">_2 -> <"[$n name]">_ {guard x==<"[expr round(ceil([[$v wcet] value]*$coef))]">; assign <'
	if {![llength $conflicts]} {'>x:=0, <'}'>mutex_<"$cn">[<"$m">]:= false;},
	val_<"[$n name]">_2 -> <'
	if {[llength $activities]} {'>finish<'
	} else {'>receive<'}'> {guard x==<"[expr round(ceil([[$v wcet] value]*$coef))]">; assign mutex_<"$cn">[<"$m">]:= false;},<'
} else {'>{ guard req_<"$cn">==<"[$n name]">_<"$cn">; assign x:= 0; },
	val_<"[$n name]"> -> <"[$n name]">_ {guard x==<"[expr round(ceil([[$v wcet] value]*$coef))]">; <'
	if {![llength $conflicts]} {'>assign x:=0; <'}'>},
	val_<"[$n name]"> -> <'
	if {[llength $activities($index)]} {'>finish<'
	} else {'>receive<'}'> {guard x==<"[expr round(ceil([[$v wcet] value]*$coef))]">; },<'}}
	} else {'>
	
	decode -> <"[$n name]">_ {guard req_<"$cn">==<"[$n name]">_<"$cn">; <'
	if {![llength $conflicts]} {'>assign x:= 0; <'}'>},<'}'>
	
	<"[$n name]">_ -> <'
	if {[llength $conflicts]} {'> <"[$n name]">_2 {guard <'
	set mutex [mutex-indexes $comp $n]
	set mutexabd [lreplace $mutex end end]
	set mutex2 [list]
	foreach c [$n codels] {
	if {[llength [$c mutex]]} {
	foreach m [mutex-indexes $comp $c] {
		if {$m != [lindex [mutex-indexes $comp $c] end]} {
			lappend mutex2 $m}}}}
	set mutexabd [concat $mutexabd $mutex2]
	set mutexabd [lsort -unique $mutexabd]
	set mutex [concat $mutexabd [lindex $mutex end]]
foreach m $mutex {
	if {$m != [lindex $mutex end]} {'>!mutex_<"$cn">[<"[lindex $m 0]">]/*in conflict with <"[lindex $m 1]">_<"[lindex $m 2]">*/<'
	if {$m != [lindex $mutex end-1]} {'> && <'}}}'>; sync exe!; assign x:=0, mutex_<"$cn">[<"$m">]:= true; },
	<"[$n name]">_2 -> <'
	if {[llength $activities($index)]} {'>finish<'
	} else {'>receive<'}'> {<'
	if {[llength [$n codels]]} {'>guard x>0; <'}'>assign <'
		if {[llength [$n interrupts]]} {'>manage_intterupt_<"$cn">(req_<"$cn">, 0, tab_<"$cn">), <'}'>mutex_<"$cn">[<"$m">]:= false;},<'
	} else {
	if {[llength $activities($index)]} {'>finish<'
	} else {'>receive<'}'> {<'
	if {[llength [$n codels]]} {'>guard x>0; <'}
	if {[llength [$n interrupts]]} {'>assign manage_intterupt_<"$cn">(req_<"$cn">, 0, tab_<"$cn">); <'}'>}, <'}
	
	}

if {[llength $activities($index)]} {'>
	
	finish -> wait { guard shut_<"$cn">;  },
	wait -> shutdown { guard off_signal_<"$cn">(tab_<"$cn">); sync exe!; assign launch_serv_<"$cn">(tab_<"$cn">);  };<'
	} else {'>
receive -> shutdown { sync recv_<"$cn">?; assign shut_<"$cn">:= true; };<'}'>

}
	
			
<'
set last "noperm"
set ind 0
foreach t [$comp tasks] {
	if {![catch {$t period}]} {'>

process Timer_<"[$t name]">_<"$cn"> (bool &tick_<"[$t name]">_<"$cn">, urgent chan &shuttimer_<"[$t name]">_<"$cn"><'
if {$last != "noperm"} {'>, broadcast chan &end_spawn_<"$last">_<"$cn"><'}'>) {
clock x;
state shutdown, start {x <= <"[expr round(ceil([[$t period] value]*$coef))]">}<'
if {$last != "noperm"} {'>, idle<'}'>;
init <'if {$last != "noperm"} {'>idle<'
} else {'>start<'}'>;
trans
	start -> start { guard x==<"[expr round(ceil([[$t period] value]*$coef))]">; assign tick_<"[$t name]">_<"$cn">:= true, x:=0;  },
	start -> shutdown { sync shuttimer_<"[$t name]">_<"$cn">?;  }<'
	if {$last != "noperm"} {'>,
	idle -> start { sync end_spawn_<"$last">_<"$cn">?; assign x:=0; }<'}'>;
}<'}'>

process Manager_<"[$t name]">_<"$cn"> (urgent chan &exe, bool &lock_<"[$t name]">_<"$cn">, bool &shut_<"$cn"><'
if {$t in $go($index)} {'>, urgent broadcast chan &go_<"[$t name]">_<"$cn"><'}
if {[llength [$t services]]} {'>, urgent chan &begin_<"[$t name]">_<"$cn">, urgent chan &inter_<"[$t name]">_<"$cn">, cell_<"$cn"> &turn_<"[$t name]">_<"$cn"> , cell_<"$cn"> &tab_<"$cn">[size_<"$cn">], bool &finished_<"[$t name]">_<"$cn">, bool &sched_<"$cn"><'}
if {![catch {$t period}]} {'>, urgent chan &shuttimer_<"[$t name]">_<"$cn">, bool &tick_<"[$t name]">_<"$cn"><'}'>) {

<'	
if {[llength [$t services]]} {'>
int i:= <"$ind">;
int j:= <"$ind">;<'}'>

state start, manage, shutdown;
init start;

trans
    start -> shutdown { guard shut_<"$cn"><'
	if {![catch {$t period}]} {'> && tick_<"[$t name]">_<"$cn"><'}
	if {[llength [$t services]]} {'> && next_<"$cn">(i,tab_<"$cn">,<"[expr $ind + [expr [llength [$t services]]*2]]">)==size_<"$cn">-1<'}'><'
	if {![catch {$t period}]} {'>; sync shuttimer_<"[$t name]">_<"$cn">!<'}'>;  },
	
<'if {[llength [$t services]]} {'>
    manage -> manage { guard lock_<"[$t name]">_<"$cn"> && i<size_<"$cn">-1; sync inter_<"[$t name]">_<"$cn">!; assign update_<"$cn"> (finished_<"[$t name]">_<"$cn">, j, tab_<"$cn">),
j:= i, i:= next_<"$cn"> (i+1, tab_<"$cn">, <"[expr $ind + [expr [llength [$t services]]*2]]">), turn_<"[$t name]">_<"$cn"> := tab_<"$cn">[i];  },
    manage -> manage { guard lock_<"[$t name]">_<"$cn"> && i<size_<"$cn">-1; sync begin_<"[$t name]">_<"$cn">!; assign update_<"$cn"> (finished_<"[$t name]">_<"$cn">, j, tab_<"$cn">),
j:= i, i:= next_<"$cn"> (i+1, tab_<"$cn">, <"[expr $ind + [expr [llength [$t services]]*2]]">), turn_<"[$t name]">_<"$cn"> := tab_<"$cn">[i];  },<'}'>

    manage -> start { guard lock_<"[$t name]">_<"$cn"><'
	if {[llength [$t services]]} {'> && i == size_<"$cn">-1<'}'>; sync exe!;<'
	if {[llength [$t services]]} {'> assign update_<"$cn"> (finished_<"[$t name]">_<"$cn">, j, tab_<"$cn">), i:= <"$ind">, sched_<"$cn">:= sched_cntrl_<"$cn">(tab_<"$cn">), finished_<"[$t name]">_<"$cn">:= false;<'}'>  },
    start -> manage {guard !(shut_<"$cn"><'
	if {[llength [$t services]]} {'> && next_<"$cn">(i,tab_<"$cn">,<"[expr $ind + [expr [llength [$t services]]*2]]">)==size_<"$cn">-1<'}'>)<'
	if {![catch {$t period}]} {'> && tick_<"[$t name]">_<"$cn"><'}'>; <'
	if {$t in $go($index)} {'>sync go_<"[$t name]">_<"$cn">!;<'
	} else {'>sync exe!;<'}
	if {[llength [$t services]] || ![catch {$t period}]} {'>assign <'
	if {![catch {$t period}]} {'>tick_<"[$t name]">_<"$cn">:= false<'
		if {[llength [$t services]]} {'>, <'}}
	if {[llength [$t services]]} {'>i:= next_<"$cn"> (i, tab_<"$cn">, <"[expr $ind + [expr [llength [$t services]]*2]]">), turn_<"[$t name]">_<"$cn"> := tab_<"$cn">[i]<'}}'>;  };

}
	
<'if {[llength [$t codels]]} {'>

process Perm_<"[$t name]">_<"$cn"> (broadcast chan &end_spawn_<"[$t name]">_<"$cn"><'
if {$last!="noperm"} {'>, broadcast chan &end_spawn_<"$last">_<"$cn"><'}
	if {$t in $go($index)} {'>, urgent broadcast chan &go_<"[$t name]">_<"$cn">,  bool &lock_<"[$t name]">_<"$cn"><'}
	if {$confcod($index) || [llength [dotgen components]]>1} {'>, urgent chan &exe<'}
	if {$confcod($index)} {'>, bool &mutex_<"$cn">[length_mut_<"$cn">]<'}
	if {[llength [dotgen components]]>1} {'>, bool &ports[nports]<'}
	if {$t in $stops($index)} {'>, bool &shut_<"$cn"><'}'>) {

clock x;

<'if {$t in $go($index)} {'>bool shift:= true;<'}'>

state <'
set reach 0
foreach c [$t codels] {
	foreach y [$c yields] {
		if {[$y name] == "ether"} {set reach 1
			break}}
			if {$reach} {break}}
if {$last!="noperm"} {'>unspawned<'}
set pauses [list]
foreach c [$t codels] {
	set ports [list]
		if {[llength [dotgen components]]>1} {
		set ports [mutex-ports dotgen $c]}
	foreach tr [$c triggers] {
		if {[$tr name] != "stop"} {
		if {$last!="noperm" || $tr!=[lindex [$c triggers] 0] ||  $c!=[lindex [$t codels] 0]} {'>, <'}'><"[$tr name]">_<'
		if {[llength [$c mutex]] || [llength $ports]} {'>, <"[$tr name]">_2<'}'> {x<=<"[expr round(ceil([[$c wcet] value]*$coef))]">}<'}}
		foreach y [$c yields] {
			if {[$y kind] == "pause event"} {
				lappend pauses $y}}}
set pauses [lsort -unique $pauses]
foreach p $pauses {'>, pause_<"[$p name]">_<'}
if {$reach} {'>, ether_<'}'>;
init <'
if {$last!="noperm"} {'>unspawned<'
	} else {'>start_<'}'>;
	
trans

<'if {$last!="noperm"} {'>
	unspawned -> start_ {sync end_spawn_<"$last">_<"$cn">?; assign x:=0; }, <'}
	
foreach p $pauses {'>
	
	pause_<"[$p name]">_ -> <"[$p name]">_ {sync go_<"[$t name]">_<"$cn">?; assign shift:= false, lock_<"[$t name]">_<"$cn">:= false, x:=0; },

<'}

foreach c [$t codels] {
	set ports [list]
		if {[llength [dotgen components]]>1} {
		set ports [mutex-ports dotgen $c]}
	foreach tr [$c triggers] {
		if {[$tr name]!="stop" && [$tr name]!="start"} {
		if {[llength [$c mutex]] || [llength $ports]} {'> 
		
	<"[$tr name]">_ -> <"[$tr name]">_2 {guard <'
	set mutex [mutex-indexes $comp $c]
	foreach m $mutex {
	if {$m != [lindex $mutex end]} {'>!mutex_<"$cn">[<"[lindex $m 0]">]/*in conflict with <"[lindex $m 1]">_<"[lindex $m 2]">*/<'
	if {$m != [lindex $mutex end-1] || [llength $ports]} {'> && <'}}}
	foreach p $ports {'>!ports[<"[lindex $p 0]">] /* uses (<"[lindex $p 1]">) the port <"[lindex $p 2]"> of the component <"[lindex $p 3]"> */<'
	if {$p!=[lindex $ports end]} {'> && <'}}'>; sync exe!; assign x:=0<'
	if {[llength [$c mutex]]} {'>, mutex_<"$cn">[<"$m">]:= true<'}
	foreach p $ports {'>, ports[<"[lindex $p 0]">]:= true<'}'>; },<'
	foreach y [$c yields] {'>

	<"[$tr name]">_2 -> <'
	if {$y in $pauses} {'>pause_<'}'><"[$y name]">_ {guard x==<"[expr round(ceil([[$c wcet] value]*$coef))]">;<'
	if {[$tr name]=="start"} {'>sync end_spawn_<"[$t name]">_<"$cn">!; <'}'>assign <'
	if {$y in $pauses} {'>update_lock_<"$cn">(shift, lock_<"[$t name]">_<"$cn">)<'
		} else {
			if {$y != "ether"} {'>x:=0<'}}
			if {[llength [$c mutex]]} {'>, mutex_<"$cn">[<"$m">]:= false<'}
			foreach p $ports {'>, ports[<"[lindex $p 0]">]:= false<'}'>; },<'}
} else {
	foreach y [$c yields] {'>
	
	<"[$tr name]">_ -> <'	
	if {$y in $pauses} {'>pause_<'}'><"[$y name]">_ {guard x==<"[expr round(ceil([[$c wcet] value]*$coef))]">;<'
	if {[$tr name]=="start"} {'>sync end_spawn_<"[$t name]">_<"$cn">!; <'}'>assign <'
	if {$y in $pauses} {'>update_lock_<"$cn">(shift, lock_<"[$t name]">_<"$cn">)<'
		} else {
			if {$y != "ether"} {'>x:=0<'}}'>; },<'}}}}}
			
foreach c [$t codels] {
	set ports [list]
		if {[llength [dotgen components]]>1} {
		set ports [mutex-ports dotgen $c]}
	foreach tr [$c triggers] {
		if {[$tr name]=="start"} {
		if {[llength [$c mutex]] || [llength $ports]} {'> 
		
	<"[$tr name]">_ -> <"[$tr name]">_2 {guard <'
	set mutex [mutex-indexes $comp $c]
	foreach m $mutex {
	if {$m != [lindex $mutex end]} {'>!mutex_<"$cn">[<"[lindex $m 0]">]/*in conflict with <"[lindex $m 1]">_<"[lindex $m 2]">*/<'
	if {$m != [lindex $mutex end-1] || [llength $ports]} {'> && <'}}}
	foreach p $ports {'>!ports[<"[lindex $p 0]">] /* uses (<"[lindex $p 1]">) the port <"[lindex $p 2]"> of the component <"[lindex $p 3]"> */<'
	if {$p!=[lindex $ports end]} {'> && <'}}'>; sync exe!; assign x:=0<'
	if {[llength [$c mutex]]} {'>, mutex_<"$cn">[<"$m">]:= true<'}
	foreach p $ports {'>, ports[<"[lindex $p 0]">]:= true<'}'>; },<'
	foreach y [$c yields] {'>

	<"[$tr name]">_2 -> <'
	if {$y in $pauses} {'>pause_<'}'><"[$y name]">_ {guard x==<"[expr round(ceil([[$c wcet] value]*$coef))]">;<'
	if {[$tr name]=="start"} {'>sync end_spawn_<"[$t name]">_<"$cn">!; <'}'>assign <'
	if {$y in $pauses} {'>update_lock_<"$cn">(shift, lock_<"[$t name]">_<"$cn">)<'
		} else {
			if {$y != "ether"} {'>x:=0<'}}
			if {[llength [$c mutex]]} {'>, mutex_<"$cn">[<"$m">]:= false<'}
			foreach p $ports {'>, ports[<"[lindex $p 0]">]:= false<'}'>; };<'}
} else {
	foreach y [$c yields] {'>
	
	<"[$tr name]">_ -> <'	
	if {$y in $pauses} {'>pause_<'}'><"[$y name]">_ {guard x==<"[expr round(ceil([[$c wcet] value]*$coef))]">;<'
	if {[$tr name]=="start"} {'>sync end_spawn_<"[$t name]">_<"$cn">!; <'}'>assign <'
	if {$y in $pauses} {'>update_lock_<"$cn">(shift, lock_<"[$t name]">_<"$cn">)<'
		} else {
			if {$y != "ether"} {'>x:=0<'}}'>; };<'}}}}}'>


}

<'}
foreach s [$t services] {'>

process <"[$s name]">_<"$cn">_ (urgent chan &begin_<"[$t name]">_<"$cn">, urgent chan &inter_<"[$t name]">_<"$cn">, bool &lock_<"[$t name]">_<"$cn">,
 const int instance, cell_<"$cn"> &turn_<"[$t name]">_<"$cn">, bool &finished_<"[$t name]">_<"$cn"><'
 if {$confcod($index) || [llength [dotgen components]]>1} {'>, urgent chan &exe<'}
 if {$confcod($index)} {'>, bool &mutex_<"$cn">[length_mut_<"$cn">]<'}
 if {[llength [dotgen components]]>1} {'>, bool &ports[nports]<'}'>) {

clock x;

state <'
set stop 0
set pauses [list]
foreach c [$s codels] {
	set ports [list]
		if {[llength [dotgen components]]>1} {
		set ports [mutex-ports dotgen $c]}
	foreach tr [$c triggers] {'><"[$tr name]">_<'
		if {[$tr name] == "stop"} {set stop 1}
		if {[llength [$c mutex]] || [llength $ports]} {'>, <"[$tr name]">_2<'}'> {x<=<"[expr round(ceil([[$c wcet] value]*$coef))]">}, <'}
		foreach y [$c yields] {
			if {[$y kind] == "pause event"} {
				lappend pauses $y}}}
set pauses [lsort -unique $pauses]
foreach p $pauses {'>pause_<"[$p name]">_, <'}'>ether_; 

init ether_;

trans
	ether_ -> start_ {guard turn_<"[$t name]">_<"$cn">.name == <"[$s name]">_<"$cn"> && turn_<"[$t name]">_<"$cn">.inst == instance && turn_<"[$t name]">_<"$cn">.status == RUN_<"$cn">;
		sync begin_<"[$t name]">_<"$cn">?; assign lock_<"[$t name]">_<"$cn">:= false, x:= 0; },<'
		
foreach c [$s codels] {
	set ports [list]
		if {[llength [dotgen components]]>1} {
		set ports [mutex-ports dotgen $c]}
	foreach tr [$c triggers] {
		if {[llength [$c mutex]] || [llength $ports]} {'>  
	
	<"[$tr name]">_ -> <"[$tr name]">_2 {guard <'
	set mutex [mutex-indexes $comp $c]
	foreach m $mutex {
	if {$m != [lindex $mutex end]} {'>!mutex_<"$cn">[<"[lindex $m 0]">]/*in conflict with <"[lindex $m 1]">_<"[lindex $m 2]">*/<'
	if {$m != [lindex $mutex end-1] || [llength $ports]} {'> && <'}}}
	foreach p $ports {'>!ports[<"[lindex $p 0]">] /* uses (<"[lindex $p 1]">) the port <"[lindex $p 2]"> of the component <"[lindex $p 3]"> */<'
	if {$p!=[lindex $ports end]} {'> && <'}}'>; sync exe!; assign x:=0<'
	if {[llength [$c mutex]]} {'>, mutex_<"$cn">[<"$m">]:= true<'}
	foreach p $ports {'>, ports[<"[lindex $p 0]">]:= true<'}'>; },<'
	foreach y [$c yields] {'>

	<"[$tr name]">_2 -> <'
	if {$y in $pauses} {'>pause_<'}'><"[$y name]">_ {guard x==<"[expr round(ceil([[$c wcet] value]*$coef))]">; assign <'
	if {$y in $pauses || [$y name] == "ether"} {'>lock_<"[$t name]">_<"$cn">:= true<'
		if {[$y name] == "ether"} {'>, finished_<"[$t name]">_<"$cn">:= true<'}
		} else {'>x:=0<'}
		if {[llength [$c mutex]]} {'>, mutex_<"$cn">[<"$m">]:= false<'}
		foreach p $ports {'>, ports[<"[lindex $p 0]">]:= false<'}'>; },<'}
		
} else {
	foreach y [$c yields] {'>
		
	<"[$tr name]">_ -> <'
	if {$y in $pauses} {'>pause_<'}'><"[$y name]">_ {guard x==<"[expr round(ceil([[$c wcet] value]*$coef))]">; assign <'
	if {$y in $pauses || [$y name] == "ether"} {'>lock_<"[$t name]">_<"$cn">:= true<'
		if {[$y name] == "ether"} {'>, finished_<"[$t name]">_<"$cn">:= true<'}
		} else {'>x:=0<'}'>; },<'}}}}
		
foreach p $pauses {'>
	
	pause_<"[$p name]">_ -> <"[$p name]">_ {guard turn_<"[$t name]">_<"$cn">.name == <"[$s name]">_<"$cn"> && turn_<"[$t name]">_<"$cn">.inst == instance && turn_<"[$t name]">_<"$cn">.status == RUN_<"$cn">;
		sync begin_<"[$t name]">_<"$cn">?; assign lock_<"[$t name]">_<"$cn">:= false, x:= 0; },<'}'>
		
	/* interruption routine */	<'
if {$stop} {
	
	foreach p $pauses {'>
	
	pause_<"[$p name]">_ -> stop_ {guard turn_<"[$t name]">_<"$cn">.name == <"[$s name]">_<"$cn"> && turn_<"[$t name]">_<"$cn">.inst == instance && turn_<"[$t name]">_<"$cn">.status == STOP_<"$cn">;
		sync inter_<"[$t name]">_<"$cn">?; assign lock_<"[$t name]">_<"$cn">:= false, x:= 0; },<'}'>
	
	
	ether_ -> stop_ {guard turn_<"[$t name]">_<"$cn">.name == <"[$s name]">_<"$cn"> && turn_<"[$t name]">_<"$cn">.inst == instance && turn_<"[$t name]">_<"$cn">.status == STOP_<"$cn">;
		sync inter_<"[$t name]">_<"$cn">?; assign lock_<"[$t name]">_<"$cn">:= false, x:= 0; };<'
	} else {
	
	foreach p $pauses {'>
	
	pause_<"[$p name]">_ -> ether_ {guard turn_<"[$t name]">_<"$cn">.name == <"[$s name]">_<"$cn"> && turn_<"[$t name]">_<"$cn">.inst == instance && turn_<"[$t name]">_<"$cn">.status == STOP_<"$cn">;
		sync inter_<"[$t name]">_<"$cn">?; assign finished_<"[$t name]">_<"$cn">:= true; },<'}'>
	
	ether_ -> ether_ {guard turn_<"[$t name]">_<"$cn">.name == <"[$s name]">_<"$cn"> && turn_<"[$t name]">_<"$cn">.inst == instance && turn_<"[$t name]">_<"$cn">.status == STOP_<"$cn">;
		sync inter_<"[$t name]">_<"$cn">?; assign finished_<"[$t name]">_<"$cn">:= true; };<'}'>
	
}

<'}
	
incr ind [expr [llength [$t services]]*2]
if {[llength [$t codels]]} {set last [$t name]}}'>

<'if {$comp == [lindex [dotgen components] end]} {'> 

process Urgency(urgent chan &exe) {

state
    wait;
init wait;
trans
    wait -> wait { sync exe?;  };
}

<'}

if {[llength [dotgen components]] == 1} {'>
	
process client(chan &recv_<"$cn"><'
if {[llength $activities($index)]} {'>, urgent chan &recv_urg_<"$cn"><'}'>, int[<'
if {[llength $activities($index)]} {'>none<'
		} else {'>kill<'}'>_<"$cn">,nserv_<"$cn">-1] &req_<"$cn">) {
int[none_<"$cn">,nserv_<"$cn">-1] temp;
state
    start;
init start;
trans
    start -> start { select temp : int [kill_<"$cn">,nserv_<"$cn">-1]; sync recv_<"$cn">!; assign req_<"$cn">:= temp;  }<'
    if {[llength $activities($index)]} {'>,
		
    start -> start { select temp : int [none_<"$cn">,nserv_<"$cn">-1]; sync recv_urg_<"$cn">!; assign req_<"$cn">:= temp;  }<'}'>;
}

	
<'}
incr index}

if {[llength [dotgen components]] > 1} {'>
	
process client() {
	// adapt the client behavior
state
    start;
init start;
trans
    start -> start {};
}

	
<'}'>

/* instantiations */

<'set index 0
foreach comp [dotgen components] {
set cn [$comp name]'>

/* <"$cn"> */

CT_<"$cn">:= control_<"$cn">(recv_<"$cn">, exe, shut_<"$cn"><'lappend compos [join [list "CT" $cn] _]
if {[llength $activities($index)]} {'>, recv_urg_<"$cn">, tab_<"$cn">, sched_<"$cn"><'}
if {$lastperm($index) > -1} {'>, end_spawn_<"[[lindex [$comp tasks] $lastperm($index)] name]">_<"$cn"><'}'>, req_<"$cn"><'
if {[llength $services($index)] && $confcod($index)} {'>, mutex_<"$cn"><'}'>);

<'set last "noperm"
foreach t [$comp tasks] {
	if {![catch {$t period}]} {
	lappend compos [join [list "timer" [$t name] $cn] _]'>

timer_<"[$t name]">_<"$cn">:= Timer_<"[$t name]">_<"$cn"> (tick_<"[$t name]">_<"$cn">, shuttimer_<"[$t name]">_<"$cn"><'
if {$last != "noperm"} {'>, end_spawn_<"$last">_<"$cn"><'}'>); 
<'}'>

Man_<"[$t name]">_<"$cn">:= Manager_<"[$t name]">_<"$cn"> (exe, lock_<"[$t name]">_<"$cn">, shut_<"$cn"><'lappend compos [join [list "Man" [$t name] $cn] _]
if {$t in $go($index)} {'>, go_<"[$t name]">_<"$cn"><'}
if {[llength [$t services]]} {'>, begin_<"[$t name]">_<"$cn">, inter_<"[$t name]">_<"$cn">, turn_<"[$t name]">_<"$cn"> , tab_<"$cn">, finished_<"[$t name]">_<"$cn">, sched_<"$cn"><'}
if {![catch {$t period}]} {'>, shuttimer_<"[$t name]">_<"$cn">, tick_<"[$t name]">_<"$cn"><'}'>); 
	
<'if {[llength [$t codels]]} {
	lappend compos [join [list "Perm" "act" [$t name] $cn] _]'>

Perm_act_<"[$t name]">_<"$cn">:= Perm_<"[$t name]">_<"$cn"> (end_spawn_<"[$t name]">_<"$cn"><'
if {$last!="noperm"} {'>, end_spawn_<"$last">_<"$cn"><'}
	if {$t in $go($index)} {'>, go_<"[$t name]">_<"$cn">,  lock_<"[$t name]">_<"$cn"><'}
	if {$confcod($index) || [llength [dotgen components]]>1} {'>, exe<'}
	if {$confcod($index)} {'>, mutex_<"$cn"><'}
	if {[llength [dotgen components]]>1} {'>, ports<'}
	if {$t in $stops($index)} {'>, shut_<"$cn"><'}'>);


<'}
foreach s [$t services] {
	lappend compos [join [list [$s name] "1" $cn] _]
	lappend compos [join [list [$s name] "2" $cn] _]'>

<"[$s name]">_1_<"$cn">:= <"[$s name]">_<"$cn">_ (begin_<"[$t name]">_<"$cn">, inter_<"[$t name]">_<"$cn">, lock_<"[$t name]">_<"$cn">,
 1, turn_<"[$t name]">_<"$cn">, finished_<"[$t name]">_<"$cn"><'
 if {$confcod($index) || [llength [dotgen components]]>1} {'>, exe<'}
 if {$confcod($index)} {'>, mutex_<"$cn"><'}
 if {[llength [dotgen components]]>1} {'>, ports<'}'>); 
 
<"[$s name]">_2_<"$cn">:= <"[$s name]">_<"$cn">_ (begin_<"[$t name]">_<"$cn">, inter_<"[$t name]">_<"$cn">, lock_<"[$t name]">_<"$cn">,
 2, turn_<"[$t name]">_<"$cn">, finished_<"[$t name]">_<"$cn"><'
 if {$confcod($index) || [llength [dotgen components]]>1} {'>, exe<'}
 if {$confcod($index)} {'>, mutex_<"$cn"><'}
 if {[llength [dotgen components]]>1} {'>, ports<'}'>); 

<'}
	
if {[llength [$t codels]]} {set last [$t name]}}
incr index}'>

urg:= Urgency(exe);

<'if {[llength [dotgen components]] == 1} {'>

cl:= client(recv_<"$cn"><'
if {[llength $activities(0)]} {'>, recv_urg_<"$cn"><'}'>, req_<"$cn">);

<'} else {'>
cl:= client();

<'}'>

system urg, cl<'
foreach c $compos {'>, <"$c"><'}'>;





