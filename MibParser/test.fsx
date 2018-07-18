

#load "Program.fs"

let file = Mib.readFile "ssm5000.mib"
//let nodes = Mib.getNodes file "PolarisSsm-1" [|1;3;6;1;4;1;31339;42080;20;1|]
let node  = Mib.parseNode file "Polaris-System" [||]


do Mib.writeFile "system.py" (Mib.snmpFieldClass + Mib.nodeToPythonClass node)
//printfn "%A" (Mib.nodeToPythonClass node)
//printfn "%A" node
//printfn "%A" (Mib.oidToString [|1;2;3|])
