let rec fact =
    fn x ->
        if x < 1 then 1
        else x * fact (x-1)

{ putn (fact 10); nl () }


/*
let rec loop = fn n m ->
    if n < m then {
        puts "fact ";
        putn n;
        puts " = ";
        putn (fact n);
        loop (n+1) m
    } else
        ()
in
loop 10
*/
