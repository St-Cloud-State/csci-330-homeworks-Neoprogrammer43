(defun merge-sort (list)
  "Sorts LIST in ascending order using the merge sort algorithm.
   This function recursively divides the list into sublists and merges them back in sorted order."
  (if (or (null list) (null (cdr list)))  ; Base case: if the list is empty or contains a single element
      list  ; Return the list as it is sorted
      (multiple-value-bind (left right) (partition list)  ; Split the list into two sublists
        (merge-lists (merge-sort left) (merge-sort right)))))  ; Recursively sort each sublist and merge them

(defun merge-lists (left right)
  "Merges two sorted lists, LEFT and RIGHT, into a single sorted list.
   This function compares the first elements of both lists and combines them in order."
  (cond ((null left) right)  ; If LEFT is empty, return RIGHT as the result
        ((null right) left)  ; If RIGHT is empty, return LEFT as the result
        ((<= (first left) (first right))  ; If the first element of LEFT is smaller or equal to the first element of RIGHT
         (cons (first left) (merge-lists (rest left) right)))  ; Add the first element of LEFT and merge the rest
        (t  ; Otherwise, the first element of RIGHT is smaller
         (cons (first right) (merge-lists left (rest right))))))  ; Add the first element of RIGHT and merge the rest

(defun partition (list)
  "Splits LIST into two sublists by taking alternating elements.
   Returns two values: the first sublist and the second sublist."
  (if (null list)  ; If the list is empty
      (values nil nil)  ; Return two empty sublists
      (if (null (cdr list))  ; If the list contains only one element
          (values (list (car list)) nil)  ; Return a list with the first element and an empty second list
          (multiple-value-bind (left right) (partition (cddr list))  ; Recursively split the rest of the list
            (values (cons (car list) left)  ; Add the first element of the list to the first sublist
                    (cons (cadr list) right))))))  ; Add the second element of the list to the second sublist
