(defun merge-lists (left right)
  "Merge two sorted lists LEFT and RIGHT into one sorted list.
   Compares the first elements of both lists, appends the smaller element to the result,
   and recursively merges the rest of the lists."
  (cond ((null left) right)  ; If LEFT is empty, return RIGHT as is
        ((null right) left)  ; If RIGHT is empty, return LEFT as is
        ((<= (first left) (first right))  ; If the first element of LEFT is smaller or equal to the first element of RIGHT
         (cons (first left) (merge-lists (rest left) right)))  ; Add the first element of LEFT and recursively merge the rest
        (t  ; Otherwise, the first element of RIGHT is smaller
         (cons (first right) (merge-lists left (rest right))))))  ; Add the first element of RIGHT and recursively merge the rest

(defun make-pairs (lst)
  "Partition LST into sorted pairs.
   Each pair is sorted in ascending order.
   If LST has an odd number of elements, the last sublist contains a single element."
  (cond ((null lst) nil)  ; If the list is empty, return an empty list
        ((null (cdr lst)) (list lst))  ; If the list has only one element, return the list as is
        (t  ; Otherwise, create pairs by taking two consecutive elements
         (let ((a (car lst))  ; First element of the pair
               (b (cadr lst)))  ; Second element of the pair
           (cons (if (<= a b)  ; If the first element is smaller or equal to the second
                     (list a b)  ; Return the pair as is
                     (list b a))  ; Otherwise, swap the elements
                 (make-pairs (cddr lst)))))))  ; Recursively process the rest of the list

(defun merge-pass (lists)
  "Merge adjacent sorted sublists in LISTS.
   This function merges pairs of lists and returns a new list of sorted sublists after one merge pass."
  (cond ((null lists) nil)  ; If the list of sublists is empty, return nil
        ((null (cdr lists)) lists)  ; If there is only one sublist, return it as is
        (t  ; Otherwise, merge the first two sublists, then recursively merge the rest
         (cons (merge-lists (car lists) (cadr lists))  ; Merge the first two sublists
               (merge-pass (cddr lists))))))  ; Recursively merge the remaining sublists

(defun bottom-up-mergesort (lst)
  "Sort LST using a bottom-up merge sort.
   First, partition LST into sorted pairs, then repeatedly merge adjacent sublists until one sorted list remains."
  (if (null lst)  ; If the list is empty, return nil
      nil
      (let ((sublists (make-pairs lst)))  ; Partition the list into sorted pairs
        (labels ((merge-all (lists)  ; Local recursive function to merge all sublists
                   (if (null (cdr lists))  ; If there is only one sublist, return it
                       (car lists)
                       (merge-all (merge-pass lists)))))  ; Otherwise, recursively merge adjacent sublists
          (merge-all sublists)))))  ; Start the merge process with the initial sorted pairs



