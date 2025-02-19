(defun insert-item (x sorted)
  "Inserts X into the sorted list SORTED 
This function recursively finds the correct insertion point for X."
  (cond
    ((null sorted) (list x))              ; Terminate: no elements remain, so return x
    ((< x (first sorted))
     (cons x sorted))                     ; Terminate: x is less than the first element.
    (t (cons (first sorted)
             (insert-item x (rest sorted))))))

(defun insertion-sort (lst)
  "Sort LST in ascending order using insertion sort.
Maintains two lists:
  - sorted: items that are already processed
  - unsorted: items that need to be processed
Moves an item from unsorted to sorted in each call
Terminates when the unsorted list is empty."
  (labels ((insort (sorted unsorted)
             (if (null unsorted)
                 sorted                    ; Termination: no more items left to insert
                 (let ((next (first unsorted)))                   ;; Insert NEXT into the sorted list.
                   (insort (insert-item next sorted) (rest unsorted))))))
    (insort nil lst)))  ; Start with sorted = nil and unsorted = lst)