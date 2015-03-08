type Direction = 
    | Right | Down | Left | Up 

type Goal = Direction

type Alternative = Direction

type Plan = Goal * Alternative

type Node = int * int

let nextPlan = function
    | (Left, Down) -> (Up, Left) 
    | (Up, Left) -> (Right, Up)
    | (Right, Up) -> (Down, Right)
    | (Down, Right) -> (Left, Down)
    | _ -> failwith "Uknown plan"

let nodeInDirection (i, j) = function
    | Right -> (i + 1, j)
    | Down -> (i, j - 1)
    | Left -> (i - 1, j)
    | Up -> (i, j + 1)

type Robot = { 
    node : Node; 
    counter : int;
    plan : Plan; 
    history : Map<Node, int> }

let move (robot : Robot) =
    let (goal, alternative) = robot.plan
    let goalNode = nodeInDirection robot.node goal
    let (nextNode, nextPlan) = 
        if Map.containsKey goalNode robot.history then  
            (nodeInDirection robot.node alternative, robot.plan)
        else
            (goalNode, nextPlan robot.plan)
    let counter = robot.counter + 1
    let history = Map.add nextNode counter robot.history
    { node = nextNode; counter = counter; plan = nextPlan; history = history }

let startingHistory = Map.empty |> Map.add (0,0) 1 |> Map.add (1,0) 2
let startingNode = (1,0)
let startingPlan = (Left, Down)
let startingCounter = 2

let robot = 
    { node = startingNode;
      counter = startingCounter; 
      plan = startingPlan;
      history = startingHistory }

let outOfBounds robot =
    let (x, y) = robot.node
    max (abs x) (abs y) > 500

let moveUntil robot doesThis =
    let rec loop robot = 
        if robot |> doesThis then
            robot
        else
            loop (move robot)
    loop robot

let spiral = 
    let robot = moveUntil robot outOfBounds
    robot.history

let isOnEitherDiagonal (x, y) =
    (abs x) = (abs y)

let euler028 = 
    spiral
    |> Map.filter (fun (x,y) _ -> isOnEitherDiagonal (x, y))
    |> Map.fold (fun sum _ n -> sum + n) 0

System.Console.WriteLine euler028