  (* 定义 completion 函数 *)
  let completion bool_list n =
    (* 获取列表长度 *)
    let list_length = List.length bool_list in
  
    (* 如果 n 大于列表长度，扩展列表 *)
    if n > list_length then
      (* 计算需要扩展的元素数量 *)
      let rec extend_list remaining current_list =
        if remaining > 0 then
          extend_list (remaining - 1) (current_list @ [false]) (* 添加 false 到列表末尾 *)
        else
          current_list
      in
      extend_list (n - list_length) bool_list (* 开始扩展列表 *)
  
    (* 如果 n 小于列表长度，切割列表 *)
    else if n < list_length then
      let rec cut_list i = function
        | [] -> [] (* 如果列表为空或者我们已经取出需要的元素，则返回空列表 *)
        | h :: t when i < n -> h :: cut_list (i + 1) t (* 如果 i < n，继续递归 *)
        | _ -> [] (* 其他情况，返回空列表结束递归 *)
      in
      cut_list 0 bool_list (* 开始切割列表 *)
  
    (* 如果 n 等于列表长度，直接返回原列表 *)
    else
      bool_list
  
  let int64_list = [0L; Int64.shift_left 1L 36];;
  let result = process_int64_list int64_list;;
  let result_str = string_of_bool_list result;;
  Printf.printf "%s\n\n" result_str;;
  
  
  let new_result = completion result 4;;
  let nnew_result = string_of_bool_list new_result;;
  Printf.printf "%s\n\n" nnew_result;;
  
  let more_result = completion result 105;;
  let mmore_result = string_of_bool_list more_result;;
  Printf.printf "%s\n\n" mmore_result;;