module ADT.Stack (Stack, empty, isEmpty, push, top, pop) where



--A more traditional abstract data type **completely**
-- hides the internal structure, or representation, of data. 
-- The following example illustrates this more traditional form of abstract data type.


empty :: Stack a
isEmpty :: Stack a -> Bool
push :: a -> Stack a -> Stack a
top :: Stack a -> a
pop :: Stack a -> (a,Stack a)
 
newtype Stack a = StackImpl [a] -- opaque!
empty = StackImpl []
isEmpty (StackImpl s) = null s
push x (StackImpl s) = StackImpl (x:s)
top (StackImpl s) = head s
pop (StackImpl (s:ss)) = (s,StackImpl ss)



