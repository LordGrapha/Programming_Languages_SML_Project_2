fun prioridad prop = 
  case prop of
        constante false             => 6
    |   constante true              => 6
    |   variable nombre             => 6
    |   negacion prop1              => 5
    |   conjuncion (prop1, prop2)   => 4
    |   disyuncion (prop1, prop2)   => 3
    |   implicacion (prop1, prop2)  => 2
    |   equivalencia (prop1, prop2) => 1
;

fun bonita prop = 
	case prop of
        constante false             => "false"
    |   constante true              => "true"
    |   variable nombre             => nombre
    |   negacion prop1              => "~" ^ 
          (case prioridad prop1 < 5 of 
              true => "(" ^ bonita prop1 ^ ")" 
            | false => bonita prop1
          )
    |   conjuncion (prop1, prop2)   => 
          (if   prioridad prop1 < prioridad prop  then  "(" ^ bonita prop1 ^ ")"
           else bonita prop1
          ) ^ " && " ^ 
          (if   prioridad prop2 > prioridad prop  then  bonita prop2
           else "(" ^ bonita prop2 ^ ")"
          )
    |   disyuncion (prop1, prop2)   => 
          (if   prioridad prop1 < prioridad prop  then  "(" ^ bonita prop1 ^ ")"
           else bonita prop1    
          ) ^ " || " ^ 
          (if   prioridad prop2 > prioridad prop  then  bonita prop2
           else "(" ^ bonita prop2 ^ ")"
          )
    |   implicacion (prop1, prop2)  => 
          (if   prioridad prop1 > prioridad prop  then  bonita prop1
           else "(" ^ bonita prop1 ^ ")"
          ) ^ " => " ^ 
          (if   prioridad prop2 < prioridad prop  then  "(" ^ bonita prop2 ^ ")"
           else bonita prop2 
          )
    |   equivalencia (prop1, prop2) => 
          (bonita prop1
          ) ^ " <=> " ^ 
          (if prioridad prop2 > prioridad prop then bonita prop2
           else "(" ^ bonita prop2 ^ ")"
          )
;

