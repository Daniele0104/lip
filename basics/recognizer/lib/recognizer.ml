let lang1 l = if List.length(l)>0 then List.for_all(fun x -> x = '1' || x = '0') l else false;;

let lang2 l = let y= List.hd l in List.for_all(fun x -> x = '1') (List.tl l) && (y = '0' || y = '1') ;;

let lang3 l= let y= List.hd l in 
let w = List.hd (List.rev(l)) in
if List.length(l)>1 then (List.for_all(fun x -> x = '1' || x = '0') l && y = '0' && w='0') else false;;

let lang4 l = List.for_all(fun x -> x = '1' || x = '0') l && List.length(List.filter(fun x -> x = '1')l)=2;;

let rec lang5 l = if (List.length(l)>1 && List.for_all(fun x -> x = '1' || x = '0') l )then 
  match l with
 []->true
 |_::[]->false
 |a::b::tl -> if a=b then lang5 tl else false
else false;;
    
let recognizers = [lang1;lang2;lang3;lang4;lang5]
                  
let belongsTo w = List.map (fun f -> f w) recognizers
  
