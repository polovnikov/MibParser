

open System
open System.IO
open System.Text.RegularExpressions

type MibField = {Name:string; Oid:int[]}

type MibNode = {Name:string; Nodes:list<MibNode>; Fields:list<MibField>}

type NodeData = {Name:string; Id:int}

let getNodeData (x:Match)= 
    {
    Name = (Regex.Match ( x.Value ,"[a-zA-Z0-9-]+")).Value;
    Id =  ((Regex.Match ( x.Value , "::= {[ ]+[a-zA-Z0-9-]+[ ]+([0-9]+)[ ]+}")).Groups.Item 1).ToString()
        |> int
}

type FieldData = {Name:string; Syntax:string; Id:int;} 
type Field = {Name:string; Id:int; Parent:string} 

let getStringFromMatch (x:Match) = 
    (x.Groups.Item 1).ToString()

let rec getFieldsFromFile (file:string) (fields:list<Field>) = 
    let typeMatch = Regex.Match ( file ,"([a-zA-Z0-9-]+)[ ]+OBJECT-TYPE")
    let typeIndex = typeMatch.Index
    let parentMatch = Regex.Match ( file.[typeIndex + typeMatch.Value.Length ..] ,"::= {[ ]+([a-zA-Z0-9-]+)[ ]+[0-9]+[ ]+}")
    let IdMatch = Regex.Match ( file.[typeIndex  + typeMatch.Value.Length ..] ,"::= {[ ]+[a-zA-Z0-9-]+[ ]+([0-9]+)[ ]+}")
    match typeMatch.Success with
    | true -> getFieldsFromFile file.[typeIndex + typeMatch.Value.Length + IdMatch.Index + IdMatch.Value.Length ..] 
                 (List.Cons ({Name = (getStringFromMatch typeMatch);  
                            Id = int (getStringFromMatch IdMatch);  
                            Parent = (getStringFromMatch parentMatch)}, 
                                fields))
              
    | false -> fields



let getFieldData (x:Match) = {
    Name =  getStringFromMatch (Regex.Match ( x.Value ,"([a-zA-Z0-9-]+)[ ]+OBJECT-TYPE"));
    Syntax = getStringFromMatch (Regex.Match ( x.Value ,"SYNTAX[ \t]+([a-zA-Z0-9-]+)"));
    Id = (getStringFromMatch (Regex.Match ( x.Value ,"::= {[ ]+[a-zA-Z0-9-]+[ ]+([0-9]+)[ ]+}"))) |> int;}

let printChildsCount x = 
    do printfn "%A" (Seq.length x)
    x

let getFields (file:string) (rootNode:string) (oidPrefix:int[]) (fields:list<Field>) = 
    fields
    |> List.filter (fun (x:Field) -> x.Parent.Equals(rootNode))
    |> List.map (fun (x:Field) -> {Name = x.Name; Oid = Array.append oidPrefix [|x.Id;|]})

   


let rec getNodes  (file:string) (rootNode:string) (oidPrefix:int[]) (fields: list<Field>) = 
    do printfn "%A" rootNode
    Regex.Matches(file , String.Format("[a-zA-Z0-9-]+[ ]+OBJECT IDENTIFIER ::= {{ {0} [0-9]+ }}", rootNode))
    |> Seq.cast
    |> Seq.map getNodeData
    |> Seq.map (fun x -> {Name = x.Name; 
                          Nodes = getNodes file (x.Name) (Array.append oidPrefix  [|x.Id|] ) fields; 
                          Fields = getFields file (x.Name) (Array.append oidPrefix  [|x.Id|] ) fields;})
    |> Seq.toList


let parseNode (file:string) (rootNode:string) (oidPrefix:int[]) = 
    let allFields = getFieldsFromFile file list<Field>.Empty
    {Name = rootNode; Nodes = getNodes file rootNode oidPrefix allFields; 
        Fields = getFields file rootNode oidPrefix allFields}

let readFile (fileName:string) = File.ReadAllText(fileName)

let writeFile (fileName:string) (data:string) = File.WriteAllText(fileName, data)

let firstCharacterToUpper (s:string) = (string s.[0]).ToUpper() + (s.[1..])

let normalizeClassName (x:string) = 
    (x.Replace("-", "_"))
    |> firstCharacterToUpper

let firstCharacterToLower (s:string) = (string s.[0]).ToLower() + s.[1..]

let normalizeFieldName (x:string) = 
    (x.Replace("-", "_"))
    |> firstCharacterToLower

let oidToString (oid:int[]) = 
    oid
    |> Array.toList
    |> List.map string
    |> String.concat "."

let snmpFieldClass = 
        "class SnmpField:\n" + 
        "    def __init__(self, manager, oid):\n" +
        "        self.manager = manager\n" +
        "        self.oid = oid\n" +
        "\n"+
        "    def get (self):\n" + 
        "        return self.manager.get(self.oid)\n" +
        "\n"+
        "    def set (self, v):\n" +
        "        self.manager.set(self.oid, v)\n" +
        "\n"

let rec nodeToPythonClass (node:MibNode) = 
    let classTemplate = 
        "class {CLASS_NAME}:\n" + 
        "    def __init__(self, manager, oid_prefix):\n" + 
        "        {NODES}\n" +
        "        {FIELDS}\n" + 
        "\n"
    
    ((classTemplate.Replace("{CLASS_NAME}", normalizeClassName(node.Name)))
        .Replace("{NODES}", 
        (List.map 
            (fun (n:MibNode) -> 
                String.Format("self.{0} = {1}(manager, oid_prefix)\n", normalizeFieldName(n.Name), 
                    normalizeClassName(n.Name))) node.Nodes) 
         |> String.concat "        "))
         .Replace("{FIELDS}", 
         (List.map 
            (fun (f:MibField) -> 
                String.Format("self.{0} = SnmpField(manager, oid_prefix +'.' + '{1}')\n", 
                    normalizeFieldName(f.Name), (oidToString f.Oid))) node.Fields) 
         |> String.concat "        ")
     +  ((List.map nodeToPythonClass node.Nodes) |> String.concat "" )
    

[<EntryPoint>]
let main argv = 
    let file = readFile "ssm5000.mib"
    //printfn "%A" (getFieldsFromFile file list<Field>.Empty)
    writeFile "ssm.py" (snmpFieldClass + nodeToPythonClass (parseNode file "PolarisSsm-1" [||]))
    writeFile "control_unit.py" (snmpFieldClass + nodeToPythonClass (parseNode file "PolarisControlUnit-1" [||]))
    writeFile "exciter.py" (snmpFieldClass + nodeToPythonClass (parseNode file "PolarisExciter-1" [||]))
    writeFile "amplifier.py" (snmpFieldClass + nodeToPythonClass (parseNode file "PolarisAmplifier-1" [||]))
    0
