fun fnd_conjunciones prop =
    let
        val variables = vars prop
        val n = length variables
        val lista_combinaciones_booleanas = gen_bools n
        (* generar evaluaciones de la proposición*)
        fun recorrer []                  = []  (* toque final a la generación; ya fueron generadas las filas precedentes *)
        |   recorrer (fila :: mas_filas) = 
                let
                    (* establecer una asociación entre variables y una combinación de valores booleanos (fila) *)
                    val asociacion = as_vals variables fila (* [("p", true), ("q", true)]*)
                    (* esta asociación constituye un ambiente, o contexto, para evaluar la proposición prop *)
                    val resultado_fila = evalProp asociacion prop
                in  
                    (* AGREGA A UNA LISTA TODAS LAS COMBINACIONES BOOLEANAS (filas) QUE DAN TRUE *)
                    if resultado_fila then asociacion :: recorrer mas_filas (* continuar el trabajo *)
                    else recorrer mas_filas
                end
    in
        recorrer lista_combinaciones_booleanas
    end
;


(* [("p", true), ("q", true)]*)
exception Conjuntar of string;

fun conjuntar [] = raise Conjuntar "*****ERROR: LISTA VACIA EN CONJUNTAR****"
  | conjuntar (elemento :: mas_elementos) =
    let 
      val (var, valor) = elemento
      val var2 = variable (var) (* [(variable "p", true), (variable "q", true)]*)
      val fVar = case valor of true => var2 | false => (~: var2) 
    in
      if length(mas_elementos) > 0 then fVar :&&: conjuntar mas_elementos else fVar
    end
;

exception Fnd of string;
fun fnd prop =
  let
    val conjunciones = fnd_conjunciones prop (* [  [("p", true), ("q", true)] , [("p", true), ("q", false)], ...] *)
    (* ITERA SOBRE LISTA conjunciones Y VA UNIENDO CADA ELEMENTO DENTRO DE UNA SUBLISTA CON AND Y CADA SUBLISTA CON OR *)
    fun recorrer [] = raise Fnd "*****ERROR: LISTA VACIA EN fnd****"
    |   recorrer (fila :: mas_filas) = 
            let
                val conj = conjuntar fila (* CREO UNA CONJUNCION CON LOS ELEMENTOS*)
            in  
                if length(mas_filas) > 0 then conj :||: (recorrer mas_filas) else conj
            end
  in 
    recorrer conjunciones
  end
;

