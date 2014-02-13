val departments=[1,2,3];
val dm = ListDeadMarkings ();
fun validmarking(n) =List.all(fn dept => Mark.Department'Task (dept)  n = []) departments;
fun checkValidMarking(dm) = List.filter validmarking dm;
val validmarkings= checkValidMarking(dm);
val schedule: (time * (string * TaskName) option) list ref =ref[];
val allschedules:  (time * (string * TaskName) option) list list ref =ref[];
val validpath :Node list list ref=ref[];
val currentpath :Node list ref=ref[1];
open TI;
val sleepstate: TransInst list ref= ref[];
val currentss: (Node*TransInst list) list ref=ref[];

fun findAllArcs(n) = OutArcs(n);

fun maparc arc = 
	let
 		val binding = ArcToBE arc
	in
 	case binding of
 		Bind.Department'Start_Task (_,{task,...}) => SOME("start",#1 task)
	      | Bind.Department'Completed (_,{task,...}) => SOME("stop",#1 task)
 	      | _ => NONE
	end;

fun checkSleepState(arc) =
let 
   val binding = ArcToBE arc
in
case binding of
        Bind.Department'Start_Task (_,{task,...}) => "False"
      | Bind.Department'Completed (_,{task,...}) => "True"
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
           
	   List.app (checkvalidpath(validmarkings)) nodelist
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
	    (currentpath:=(node)::(!currentpath);
           currentss:=(node,(!sleepstate))::(!currentss);
             print ("after adding node" ^ (Int.toString node) ^ "\n");
print ("hd(tl)"^(Int.toString (hd(tl(!currentpath))))^"\n");
             getSchedule(hd(tl(!currentpath)),node); 
             print ("after getting schedule" ^ (Int.toString node)^"\n");
             validpaths(node);
             schedule:=List.tl(!schedule);
             currentpath := List.tl (!currentpath);
	     print ("after taking the tail of currentpath")
             )

and checkvalidnode(node,validmarkings)=
	if validmarkings<>[] then
   	   if node=hd(validmarkings) then 
              true
    	   else 
             checkvalidnode(node,tl(validmarkings))
	else false

and checkvalidpath validmarkings node =
    	let
           val destpath=ref []
        in
	  if (length(OutArcs(node))=0) then
    		if checkvalidnode(node,validmarkings) then
     		 (print ("Inside checkvalidpath if cond" ^(Int.toString node)^"\n");  
		addNodeToPath(node,true))
     
    		else ()
	  else
  	      (print ("Inside checkvalidpath if cond" ^"\n");
		addNodeToPath(node,false))
end;


val paths=validpaths(1);
val sch = !allschedules;
val allschedules = List.length(!allschedules);	
!schedule;			












