val dm = ListDeadMarkings ();

val tms=Mark.Department'Task 1 1 +++
Mark.Department'Task 2 1 +++
Mark.Department'Task 3  1;
val completedtokens=TMS.ms tms;



fun checkValidMarking(tms,dm)=List.map(valid(tms)) dm 

and valid(tms) dm=
let 
val completedtasks=Mark.ProductionSystem'Completed 1 dm;

val node=dm
in
if (tms<>[] andalso completedtasks<>[]) then
  if length(tms)=length(hd(completedtasks)) then
    (print ("node" ^ (Int.toString node) ^"\n");
     node)
  else (print (("m here 2nd else") ^"\n");
     0)
else (print (("m here 3rd else") ^"\n");
     0)
end;





val validmarkings=checkValidMarking(completedtokens,dm);
val validdeadmarkings=List.filter(fn t=> t<>0)validmarkings;

