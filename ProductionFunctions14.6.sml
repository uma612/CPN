
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


fun DetermineConstraints (n,c:Constraints) =
if (c<>[]) andalso #2 (hd(c))=n then
  (hd(c))::DetermineConstraints(n,tl(c))
else [("",n, NoConstraint)]

fun AllconstraintsSystem()=[("2","13", Before),("10","13", Before),("13","18", Before),
	                    ("24","25", Before),("25","26", InProcess),("26","35", Before)];
fun AllconstraintsEngine()=[("1","3", Before),("1","4", Before),("1","7", Before),
                            ("7","8", Before),("8","9", InProcess),("10","11", Before),
                            ("8","12", Before),("12","14", InProcess),("14","15", Before),
                            ("15","16", InProcess),("11","17", Before),("18","20", Before),
			    ("20","21", InProcess),("19","22", Before),("21","23", Before),
                            ("23","24", InProcess)];
fun AllconstraintsDovumentation()=[("1","5", Before),("7","10", Before),("14","19", Before),
				   ("24","28", Before),("25","30", InProcess),("24","32", Before),
		                   ("26","34", Before)];



fun remove (task,ptasks) = List.filter (fn t => t<>task) ptasks;

(* Determine the mapping between orders and parts *)

fun DetermineListofTasks (partlist:parts) (tasklist:PartXTasks)=
if ((hd (partlist))= #1 tasklist) then
#2 tasklist
else if (tl(partlist)<>[]) then
DetermineListofTasks(tl(partlist)) tasklist
else empty;


(* Detremine the mapping of a part with the list of tasks *)

fun MapPartstoTasks (partlist,tasklist) =
List.concat (List.map (DetermineListofTasks (partlist)) tasklist);

(* Determine Departments ******Getting error while method call*******
fun getst()=[("1"),("2"),("6"),("13"),("18"),("25"),("26"),("35")];


fun getet()=[("3"),("4"),("7"),("8"),("9"),("11"),("12"),("14"),("15"),("16"),("17"),("20"),("21"),("22"),("23"),("24"),("33")];

fun getdt()=[("5"),("10"),("19"),("27"),("28"),("29"),("30"),("31"),("32"),("34")];


fun SystemTasks (systemlist:Tasknamelist) (task:Task)=
if ((hd (systemlist))= #1 task) then 
task
else 
SystemTasks(tl(systemlist)) task;


fun CheckDepartmentSystem (t:Tasks) = 
let
val st=getst()
in
List.map (SystemTasks (st)) t
end;

fun CheckDepartmentEngine (t:Tasks) = 
let
val et=getet()
in
List.map (SystemTasks (et)) t
end;

fun CheckDepartmentDocs (t:Tasks) =
let
val dt=getdt()
in 
List.map (SystemTasks (dt)) t
end;
*)

fun DetermineListParts(inorder,orderlist:OrderParts)=
if(inorder= #1 (hd(orderlist))) then
#2 (hd(orderlist))
else DetermineListParts(inorder, tl(orderlist));

(* Mapping of orders to parts *)

fun AllOrders () = 1`(Order(1),[Part(1),Part(14),Part(15)])++
1`(Order(2),[Part(1),Part(2)]);

(* Mapping of parts to tasks *)

fun AllParts () = 1`(Part(1),[("1",5,3),("2",4,2),("3",5,2)])++
1`(Part(2),[("4",5,2),("5",5,3),("6",5,3)])++
1`(Part(3),[("7",5,2),("8",5,3),("9",5,3)])++
1`(Part(4),[("10",5,2)])++
1`(Part(5),[("11",5,2),("12",5,2)])++
1`(Part(6),[("13",5,2)])++
1`(Part(7),[("14",5,2),("15",5,3),("16",5,3)])++
1`(Part(8),[("17",5,3),("18",5,3)])++
1`(Part(9),[("19",5,2),("20",5,3),("21",5,3),("22",5,3)])++
1`(Part(10),[("23",5,3)])++
1`(Part(11),[("24",5,3)])++
1`(Part(12),[("25",5,2),("26",5,3),("27",5,3),("28",5,3)])++
1`(Part(13),[("29",5,3),("30",5,3),("31",5,3),("32",5,3)])++
1`(Part(14),[("33",6,3),("34",5,3)])++
1`(Part(15),[("35",5,3)]);

(* Determine Departments *)
fun CheckDepartmentSystem (t:Tasks) = 
List.filter (fn t=> #1 t="1" orelse #1 t="2" orelse #1 t="6" orelse #1 t="13" orelse #1 t="18" orelse #1 t="25" orelse #1 t="26" orelse #1 t="35") t;

fun CheckDepartmentEngine (t:Tasks) = 
List.filter (fn t=> #1 t="3" orelse #1 t="4" orelse #1 t="7" orelse #1 t="8" orelse #1 t="9" orelse #1 t="11" orelse #1 t="12" orelse #1 t="14"
                    orelse #1 t="15" orelse #1 t="16" orelse #1 t="17" orelse #1 t="20" orelse #1 t="21" orelse #1 t="22" orelse #1 t="23"
                    orelse #1 t="24" orelse #1 t="33") t;

fun CheckDepartmentDocs (t:Tasks) = 
List.filter (fn t=> #1 t="5" orelse #1 t="10" orelse #1 t="19" orelse #1 t="27" orelse #1 t="28" orelse #1 t="29"
              orelse #1 t="30" orelse #1 t="31" orelse #1 t="32" orelse #1 t="34") t;

