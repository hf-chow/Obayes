open Owl_stats

let get_pdf distribution params = 
    match (distribution, params) with
    | ("normal", [mu; sigma]) ->
            (fun x -> pdf_gaussian ~mu ~sigma x)
    | ("exponential", [lambda]) ->
            (fun x -> pdf_exponential ~lambda x)
    | ("uniform", [a; b]) ->
            (fun x -> pdf_uniform ~a ~b x)
    | ("beta", [alpha; beta]) ->
            (fun x -> pdf_beta ~a:alpha ~b:beta x)
    | ("gamma", [shape; scale]) ->
            (fun x -> pdf_gamma ~shape ~scale x)
    | ("poisson", [lambda]) ->
            (fun x -> pdf_poisson ~lambda (Float.to_int x))
    | _ ->
            failwith "Invalid parameters"
