(def fun_2 {a} 
  	(if (< a 10)
		then ((sout a) (fun_2 (+ a 1))) 
	else "function completed")
) 
(fun_2 1) # рекурсивная функция, которая выводит значения от 1 до 9, а потом завершается строкой "function completed"