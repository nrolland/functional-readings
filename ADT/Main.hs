{-# LANGUAGE ScopedTypeVariables #-}
module ADT.Main where 
    
import ADT.Stack as Stack
import Data.Int
import Control.Monad

data Tree a = Nil 
            | Node { left  :: Tree a,
                     value :: a,
                     right :: Tree a }
--This type is abstract because it leaves some aspects of its structure undefined, 
--to be provided by the user of the data type. 
--This is a weak form of abstract data type.



--In this example, the type of elements contained in the tree is left open. 
--For example, a user of this data type might use it like this:

three_number_tree :: Tree Integer
three_number_tree = Node (Node Nil 1 Nil) 2 (Node Nil 3 Nil)


--with type class

class Tree' t where
    nil   :: t a
    node  :: t a -> a -> t a -> t a
    left'  :: (MonadPlus m) => t a -> m (t a)
    right' :: (MonadPlus m) => t a -> m (t a)
    value' :: (MonadPlus m) => t a -> m a

-- Note that this description is even more abstract than the description of the Tree abstract data type above.
-- This interface will also make the implementation of the tree abstract (not just the element type). 
-- This interface allows the user to change the implementation of the tree data type later easily. 
-- However, the type class does not prevent access to the underlying data representation, 
-- hence type classes provide a weaker form of abstraction than the Stack example given above.





s =  Stack.push  1 $ Stack.push  0   Stack.empty  --I never access the representation here
t = Stack.top s 

-- The "stack" example is fully abstract, but the "Tree" example is not. 
-- In particular, the implementation of the Tree operations in the interface was not made abstract. 
-- This means that the implementation cannot be changed without changes to all code that uses the type.

main :: IO ()
main = 
    print t 
   --putStrLn "Hi"
