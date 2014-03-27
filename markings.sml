val dm = ListDeadMarkings ();

val tms=Mark.Department'Task 1 1 +++
Mark.Department'Task 2 1 +++
Mark.Department'Task 3 1;
val ms=TMS.ms tms;



fun checkValidMarking(ms,dm)=List.map(valid(ms)) dm

and valid(ms) dm=
let
val completedtasks=Mark.ProductionSystem'Completed 1 dm;

val node=dm
in
if (ms<>[] andalso completedtasks<>[]) then
  if length(ms)=length(hd(completedtasks)) then
    (print ("node" ^ (Int.toString node) ^"\n");
     node)
  else (0)
else (0)
end;





val validmarkings=checkValidMarking(ms,dm);
val validdeadmarkings=List.filter(fn t=> t<>0)validmarkings;