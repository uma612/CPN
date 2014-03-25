(*fun SatConstraints (b:Tasks,c:PartialTasks,t:Task) (con:Constraint)=
   if (#2 con)= #1 t then
            if b<>[] andalso (#3 con)=Before then
                  if #1(hd(b))= (#1 con) then true
                  else SatConstraints (tl(b),c,t) con
            else if c<>[] andalso (#3 con)=InProcess then
                     if #1(hd(c)) = (#1 con) then true
                     else SatConstraints (b,tl(c),t) con 
            else if (#3 con)=NoConstraint then true
            else false
    else false;
*)
fun SatConstraints (b:Tasks,c:PartialTasks,t:Task) (con:Constraint)=
   (#2 con)= #1 t  andalso
            if b<>[] andalso (#3 con)=Before then
                  #1(hd(b))= (#1 con) orelse SatConstraints (tl(b),c,t) con
                 
            else if c<>[] andalso (#3 con)=InProcess then
                      #1(hd(c)) = (#1 con) orelse SatConstraints (b,tl(c),t) con
                      
            else (#3 con)=NoConstraint
            
    

fun CheckAllConstraints (constraints,tasks,partialtasks,task) =
List.all (SatConstraints (tasks,partialtasks,task)) constraints;

fun IsDepartmentC(t:Task)=
if #1 t="C" then 1`t else empty;

fun Taskname(t:Task) = #1 t;

fun Tasktime(t:Task) = #2 t;

fun Engineers(t:Task) = (#3 t)`Engineer;

fun IsDepartmentB(t:Task)=
if #1 t="B" then 1`t else empty;

fun IsDepartmentA(t:Task)=
if #1 t="A" orelse #1 t="D" then 1`t else empty;

fun DetermineConstraints (n) = 
if n = "B" then
[("A","B", InProcess),("C","B", InProcess)]
else if n="C" then
[("A","C", InProcess)]
else if n="D" then
[("B","D", Before)]
else 
[("",n, NoConstraint)];






fun DetermineTasks (n)= 
if n < 5 then
[("A",5,3),("B",5,2),("C",10,1),("D",10,1)]
else
[("A",5,3),("C",7,2)];

fun remove (task,ptasks) = List.filter (fn t => t<>task) ptasks;

(* Determine the mapping between orders and parts *)

fun DetermineListofTasks (partlist:parts) (tasklist:PartXTasks)=
if ((hd (partlist))= #1 tasklist) then
#2 tasklist
else if (tl(partlist)<>[]) then
DetermineListofTasks(tl(partlist)) tasklist
else empty;



(* Not used *)
fun partsForOrder(inorder):parts = 
     (
        case inorder of
         Order(1) => [Part(1), Part(2)]
      | Order(2) => [Part(2), Part(3)]
      | Order(3) => [Part(3)]
    )

(* Detremine the mapping of a part with the list of tasks *)

fun MapPartstoTasks (partlist,tasklist) =
List.concat (List.map (DetermineListofTasks (partlist)) tasklist);

(* Determine Departments *)
fun CheckDepartmentAD (t:Tasks) = 
List.filter (fn t=> #1 t="A" orelse #1 t="D") t;

fun CheckDepartmentB (t:Tasks) = 
List.filter (fn t=> #1 t="B") t;

fun CheckDepartmentCE (t:Tasks) = 
List.filter (fn t=> #1 t="C" orelse #1 t="E") t;

fun DetermineListParts(inorder,orderlist:OrderParts)=
if(inorder= #1 (hd(orderlist))) then
#2 (hd(orderlist))
else DetermineListParts(inorder, tl(orderlist));

fun AllOrders () = 1`(Order(1),[Part(1),Part(3)])++
1`(Order(2),[Part(1),Part(2)]);

fun AllParts () = 1`(Part(1),[("A",5,3),("B",4,2),("C",5,2)])++
1`(Part(2),[("C",5,2),("A",5,3)])++
1`(Part(3),[("E",6,3)]);



