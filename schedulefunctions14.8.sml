val departments=[1,2,3];
val dm = ListDeadMarkings ();

use "markings.sml";

(*
fun validmarking(n) =List.all(fn dept => Mark.Department'Task (dept)  n = []) departments;
fun checkValidMarking(dm) = List.filter validmarking dm;
val validmarkings= checkValidMarking(dm);


*)

val test=[];
(* val schedule: (time * (string * TaskName) option) list ref =ref[];
val allschedules:  (time * (string * TaskName) option) list list ref =ref[]; *)
val schedule: (time * (string * TaskName)) list ref =ref[];
val allschedules:  (time * (string * TaskName)) list list ref =ref[];
val validpath :Node list list ref=ref[];
val currentpath :Node list ref=ref[1];
open TI;
val sleepstate: TransInst list ref= ref[];
val currentss: (Node*TransInst list) list ref=ref[];

fun findAllArcs(n) = OutArcs(n);

(* fun maparc arc = 
	let
 		val binding = ArcToBE arc
	in
 	case binding of
 		Bind.Department'Start_Task (_,{task,...}) => SOME("start",#1 task)
	      | Bind.Department'Completed (_,{task,...}) => SOME("stop",#1 task)
 	      | _ => NONE
	end; *)
fun maparc arc = 
let
 val binding = ArcToBE arc
in
 case binding of
 Bind.Department'Start_Task (_,{uptask,...}) => ("starts",#1 uptask)
      | Bind.Department'Completed (_,{uptask,...}) => ("stops",#1 uptask)
       | _ => ("","")
end;
 fun checkSleepState(arc) =
let 
   val binding = ArcToBE arc
in
case binding of
        Bind.Department'Start_Task (_,{uptask,...}) => "False"
      | Bind.Department'Completed (_,{uptask,...}) => "True"
       | _ => "False"
end; 


fun validpaths(node) =
	let
	   val nodelist=[];
	   val allarcs=
		if node<1 then 
		   findAllArcs(1)
		else
		   findAllArcs(node)
	in
          (print ("Inside validpath function" ^"\n");
	  getNodeList(allarcs))
	end

and getNodeList(allarcs) =
	let 
	   val nodelist=List.map(fn arc=>DestNode(arc)) allarcs
	in 
           
	   List.app (checkvalidpath(validdeadmarkings)) nodelist
	end

and getSchedule(sourcenode,destnode)=
	let 
	   val arc= hd(ArcsInPath(sourcenode,destnode))
	in
	  (print ("sourcenode" ^ (Int.toString sourcenode) ^"destnode"^(Int.toString destnode) ^ "\n");
	   schedule:=(CreationTime(DestNode(arc)),maparc arc) :: (!schedule);
           if(checkSleepState(arc)="True") then
             sleepstate:=ArcToTI(arc)::(!sleepstate)
            else
()
          
                      
           )
	end 

and addNodeToPath(node,check) : unit =
	if check=true then
	   (
		currentpath:=(node)::(!currentpath);
		 currentss:=(node,(!sleepstate))::(!currentss);
	   	validpath:=((!currentpath)::(!validpath));
		print ("hd(tl)"^(Int.toString (hd(tl(!currentpath))))^"\n");
            	getSchedule(hd(tl(!currentpath)),node);
            	allschedules:=((!schedule)::(!allschedules));
		schedule:=List.tl(!schedule);
		
		currentpath := List.tl (!currentpath)
           )
        else
	    (
		currentpath:=(node)::(!currentpath);
           	currentss:=(node,(!sleepstate))::(!currentss);
             	(*print ("after adding node" ^ (Int.toString node) ^ "\n");
             	print ("hd(tl)"^(Int.toString (hd(tl(!currentpath))))^"\n");*)
             	getSchedule(hd(tl(!currentpath)),node); 
             	(*print ("after getting schedule" ^ (Int.toString node)^"\n");*)
             	validpaths(node);
				
             	schedule:=List.tl(!schedule);
				
             	currentpath := List.tl (!currentpath)
	     	(*print ("after taking the tail of currentpath")*)
             )

and checkvalidnode(node,validdeadmarkings)=
	if validdeadmarkings<>[] then
   	   if node=hd(validdeadmarkings) then 
              true
    	   else 
             checkvalidnode(node,tl(validdeadmarkings))
	else false

and checkvalidpath validdeadmarkings node =
    	let
           val destpath=ref []
        in
	  if (length(OutArcs(node))=0) then
    		if checkvalidnode(node,validdeadmarkings) then
     		 (print ("Inside checkvalidpath if cond" ^(Int.toString node)^"\n");  
		addNodeToPath(node,true))
     
    		else ()
	  else
  	      (print ("Inside checkvalidpath if cond" ^"\n");
		addNodeToPath(node,false))
end;


val paths=validpaths(1);

(* val sch = !allschedules; *)
val allschedules = List.length(!allschedules);	
(*			
!sleepstate;
!currentss;
!currentpath;
!validpath;*)



(* Result output file *)

(* fun scheduletoStr sch = 
let 
val sch=List.rev(sch)
in
String.concat (List.map (fn (t,(str:string,tname:TaskName))=> "Task ["^tname^"] "^str^" at "^(ModelTime.toString (t))^",\n")sch)
end;

fun dumpSchedule sch = 
let
val sch=List.rev(sch);
 val outfile = TextIO.openOut("//hallingskeid.uib.no/ube072/Settings/Desktop/GitHub/CPN/Testresults/TimeSchedules/order7constraint4.txt");
 val _ = List.app (fn sch => TextIO.output (outfile,(scheduletoStr sch)^"\n-----\n")) sch
in
TextIO.closeOut outfile
end;

dumpSchedule (sch); *)

(* val j=ref (CPN'Time.fromInt 0);
val bestpath: (time * (string * TaskName)) list ref =ref[];
fun getBestSchedule([])= (!j,!bestpath)
| getBestSchedule(paths)=
let 
val _ = print "here\n"
val (x,_)=(hd(hd(paths)));
val path=hd(paths);

val _ = print "and there\n"
in

if (!j=0)  orelse (CPN'Time.cmp(x,!j) = LESS) then
      (j:=x;
bestpath:=path;
getBestSchedule(tl(paths)))
   else 
(bestpath;
     getBestSchedule(tl(paths)))
end;

val besttime=getBestSchedule(sch);
fun besttimescheduletoStr sch = 
let 
val sch=List.rev(sch)
in
List.map (fn (t,(str:string,tname:TaskName))=> "Task ["^tname^"] "^str^" at "^(ModelTime.toString (t))^",\n")sch
end;

fun dumpbestschedule (besttime)=
let
val (_,x)=(besttime);
val outfile = TextIO.openOut("//hallingskeid.uib.no/ube072/Settings/Desktop/GitHub/CPN/Testresults/TimeSchedules/order1bestschedule.txt");
val _ = List.app (fn sch => TextIO.output (outfile,(besttimescheduletoStr x)^"\n-----\n")) sch
in
TextIO.closeOut outfile
end; *)

 





