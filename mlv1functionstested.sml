fun IsDesired n = true;

val dm = ListDeadMarkings ();

fun maparc arc = 
let
 val binding = ArcToBE arc
in
 case binding of
 Bind.Department'Start_Task (_,{task,...}) => SOME("start",#1 task)
| Bind.Department'Completed (_,{task,...}) => SOME("stop",#1 task)
 | _ => NONE
end;



val schedule = List.map (fn arc => (CreationTime(DestNode(arc)),maparc arc)) onepath;

val schedulesanitized = List.filter (fn (_,NONE) => false | _ => true) schedule;

fun validmarking(n) =List.all(fn dept => Mark.Department'Task (dept)  n = []) departments;

fun checkValidMarking(dm) = List.filter validmarking dm;

val validmarkings= checkValidMarking(dm);


SearchNodes (
EntireGraph,
fn n => (length(OutArcs(n)) = 0),
10,
fn n => n,
[],
op ::);


ListHomeMarkings();
SccReachable' (2,7);

AllReachable();

OutArcs(1);

DestNode(OutArcs(1));

addNodeToPath(node,check)=
(print ("addNodeToPath" ^ (Int.toString node) ^ "\n");
if check=true then
validpath:=(node :: (!currentpath)) :: (!validpath)
else
(currentpath:=node::(!currentpath);
 print ("after adding node" ^ (Int.toString node) ^ "\n");
 validpaths(node);
 currentpath := List.tl (!currentpath)
))



---------------- valid function --------------

fun validpaths(node)=
let
val node=if node<1 then findAllPaths(1)
else
findAllPaths(node)
in
(List.app (checkvalidpath(dm)) node;
print ("List of nodes" ^(ListFormat.listToString Int.toString node)^ "\n")
)
end

and addNodeToPath(node,check)=
(print ("addNodeToPath" ^ (Int.toString node) ^ "\n");
if check=true then
validpath:=(node :: (!currentpath)) :: (!validpath)
else
(currentpath:=node::(!currentpath);
 
 validpaths(node);
 currentpath := List.tl (!currentpath)
))

and checkvalidpath(dm) node=
    let
       val destpath=[] 
      in
if (length(OutArcs(node))=0) then
    if checkvalidnode(node,dm) then
     addNodeToPath(node,true)
    else 
     ()
else
  addNodeToPath(node,false)
end

and checkvalidnode(node,dm)=
if node=hd(dm) then
true
else checkvalidnode(node,tl(dm));

------------------------- added in sml3------------------

fun findArcForPath() validpath=
let 
val arcs=[]
in
if (length(validpath))<>[] then
arcs=ArcsInPath(hd(validpath),hd(tl(validpath)))
else
arcs=[]
end;

fun eachPath() validpathnodes=
print ("after adding node" ^ (Int.toString validpathnodes) ^ "\n");

fun getEachPath(validpath)=List.map(findArcForPath()) validpath;

checkvalidnode(node,validmarkings)=
if node=hd(validmarkings) then
true
else checkvalidnode(node,tl(validmarkings));

ArcsInPath(1,1);

