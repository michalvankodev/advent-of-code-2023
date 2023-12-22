open Core

let fib_nonrec fib n = 
  if n <= 2 then 1
  else (fib (n-1)) + (fib (n-2))
 
let memo_fib = Memo.recursive ~hashable:Int.hashable fib_nonrec

let () = 
  let result = memo_fib 1000 in
  print_endline (Printf.sprintf "Result %d" result);

