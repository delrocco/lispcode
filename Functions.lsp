;Author: Joe Del Rocco
;Date:   10/2000
;
;FUNCTION LIBRARY:
;|----NAME----------ARGS-----|
;  (elementOf     atom list)
;  (makeSet       list)
;  (insert        atom set)
;  (subset        set set)
;  (setEquality   set set)
;  (union         set set)
;  (intersection  set set)
;  (cartProduct   set set)
;  (MaxList       list)
;  (MinList       list)
;  (MaxMM         list)
;  (MinMM         list)
;|---------------------------|

;Function that tests to see if the first argument exists in the second argument.
;Tests both atoms and lists representing an element and a list. This is because this
;function returns the FIRST instance of the element in the list.
;INPUT:     elm - the element to test existence on.
;           lst - the list searched for the atom.
;RETURN:    boolean value 'NIL' or 'T' for existence of
;           element in lst.
(defun elementOf(elm lst)
   (cond ((NULL lst) NIL)
         ((EQUAL elm (CAR lst)) T)
         (T (elementOf elm (CDR lst)))
   )
)

;Function that converts a list into a set by deleting all duplicate elements.
;INPUT:     lst - the list inputed to convert.
;RETURN:    list representing a mathematical set.
(defun makeSet(lst)
   (cond ((NULL (CDR lst)) lst)
         ((elementOf (CAR lst) (CDR lst)) (makeSet (CDR lst)))
         (T (CONS (CAR lst) (makeSet (CDR lst))))
   )
)

;Function that inserts the given element into the given set..
;INPUT:     elm - the element to be inserted into set.
;           set - the set into which to insert the element.
;RETURN:    returns the new set..note if element is already in set, then the function
;           merely returns the inputed set. Also note that if the inputed set is
;           in fact not a set<having repeated elements>, then this function returns it
;           converted to a set.
(defun insert(elm set)
   (cond ((NULL elm) (makeSet set))
         (T (makeSet (CONS elm set)))
   )
)

;Function that tests to see if a given argument is a subset of the second argument.
;INPUT:     set1 - potential subset of second argument.
;           set2 - potential superset of first argument.
;RETURN:    boolean value 'NIL' or 'T' for first argument as a subset of second argument.
(defun subset(set1 set2)
   (cond ((NULL set1) T)
         ((elementOf (CAR set1) set2) (subset (CDR set1) set2))
         (T NIL)
   )
)

;Function that tests the equality between two given sets.
;INPUT:     set1 - first of two sets.
;           set2 - second of two sets.
;RETURN:    boolean value 'NIL' or 'T' for equality between the two given sets.
(defun setEquality(set1 set2)
   (cond ((subset set1 set2) (subset set2 set1))
         (T NIL)
   )
)

;Function that returns the union of two sets.
;INPUT:     set1 - the first of two sets.
;           set2 - the second of two sets.
;RETURN:    set representing the union of both inputed sets.
(defun union(set1 set2)
   (makeSet (APPEND set1 set2))
)

;Function that returns the intersection of two sets.
;INPUT:     set1 - the first of two sets.
;           set2 - the second of two sets.
;RETURN:    set representing the intersection of both inputed sets.
(defun intersection(set1 set2)
   (cond ((NULL set1) set1)
         ((NOT (elementOf (CAR set1) set2)) (intersection (CDR set1) set2))
         (T (CONS (CAR set1) (intersection (CDR set1) set2)))
   )
)

;Function that returns the cartesian product of a set containing
;a single element with a set containing any number of elements..
;This function is used with "cartProduct" to provide easier
;implementation of calculating the cartesian product of two sets
;containing any number of elements.
;INPUT:     elm - element to calc. cartesian product with set.
;           set - set to calc. cartesian product with element.
;RETURN:    returns a set containing cartesian products.
(defun cartElement(elm set)
   (cond ((OR (NULL elm) (NULL set)) NIL)
         (T (CONS (LIST elm (CAR set)) (cartElement elm (CDR set))))
   )
)

;Function that returns the cartesian product of two sets containing any number of
;elements. This function uses "cartElement" defined above.
;INPUT:     set1 - first set in calculating cart. product.
;           set2 - second set in calculating cart. product.
;RETURN:    returns a set containing cartesian products.
(defun cartProduct(set1 set2)
   (cond ((OR (NULL set1) (NULL set2)) NIL)
         (T (APPEND (cartElement (CAR set1) set2) (cartProduct (CDR set1) set2)))
   )
)

;Function for getting the max number in a list of numbers.
;Input:  lst - list of numbers.
(defun MaxList(lst)
	(cond
		((NULL (CDR lst))  (CAR lst))
		((> (CAR lst)(MaxList (CDR lst)))  (CAR lst))
		(T  (MaxList (CDR lst)))
	)
)

;Function for getting the min number in a list of numbers.
;Input:  lst - list of numbers.
(defun MinList(lst)
	(cond
		((NULL (CDR lst))  (CAR lst))
		((< (CAR lst)(MinList (CDR lst)))  (CAR lst))
		(T  (MinList (CDR lst)))
	)
)

;Function for returning the max of a minmax tree.
;Input:  lst - the list of nodes in tree form
(defun MaxMM(lst)
	(cond
		((NULL (CAR lst))  -9999)
		((NUMBERP (CAR lst))  (MaxList (LIST (CAR lst)(MaxMM (CDR lst)))))
		(T  (MaxList (LIST (MinMM (CAR lst))(MaxMM (CDR lst)))))
	)
)

;Function for returning the min of a minmax tree.
;Input:  lst - the list of nodes in tree form
(defun MinMM(lst)
	(cond
		((NULL (CAR lst))  9999)
		((NUMBERP (CAR lst))  (MinList (LIST (CAR lst)(MinMM (CDR lst)))))
		(T  (MinList (LIST (MaxMM (CAR lst))(MinMM (CDR lst)))))
	)
)
