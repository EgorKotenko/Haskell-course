data Form a = Zero | Const a | X | Neg (Form a) | Mult (Form a) (Form a) | Diff (Form a) (Form a) | Sum (Form a) (Form a) | Exp (Form a) | Ln (Form a) | Log (Form a) (Form a) | Pow (Form a) (Form a) | Sin (Form a) | Cos (Form a) deriving (Show, Eq)

-- Предполагается, что пользователь вводит выражение в более-менее красивом виде
-- Полагаю, что все сокращения понятны. На всякий случай: Diff - разность

derivative :: (Eq a, Num a) => Form a -> Form a
derivative form = reduction (derivative' form)

derivative' :: (Eq a, Num a) => Form a -> Form a

derivative' Zero = Zero
derivative' (Const n) = Zero
derivative' X = Const 1
derivative' (Neg f) = Neg (derivative' f)

derivative' (Mult f g) = Sum (reduction (Mult (derivative f) g)) (reduction (Mult f (derivative g)))
derivative' (Diff f g) = Sum (derivative' f) (derivative' (Neg g))
derivative' (Sum f g) = Sum (derivative f) (derivative g)

derivative' (Exp f) = Mult (derivative f) (Exp f)
derivative' (Ln f) = Mult (derivative f) (reduction (Pow f (Const (-1))))
derivative' (Log f g) = Mult (derivative f) (reduction (Pow (reduction (Mult (f) (Ln g))) (Neg (Const 1))))
derivative' (Pow f g) = Mult (reduction (Mult g (reduction (Pow f (reduction (Diff g (Const 1))))))) (derivative f)

derivative' (Sin f) = Mult (reduction (Cos f)) (derivative f)
derivative' (Cos f) = Mult (Neg (reduction (Sin f))) (derivative f)

reduction :: (Eq a, Num a) => Form a -> Form a

reduction (Mult (Const 1) X) = X
reduction (Ln (Const 1)) = Zero
reduction (Log (Const 1) _) = Zero
reduction (Ln (Exp (Const 1))) = Const 1
reduction (Neg (Const a)) = Const (-a)

reduction (Mult _ Zero) = Zero
reduction (Mult Zero _) = Zero
reduction (Sum f Zero) = f
reduction (Sum Zero f) = f
reduction (Diff f Zero) = f
reduction (Diff Zero f) = f
reduction (Neg Zero) = Zero
reduction (Exp Zero) = Const 1
reduction (Pow _ Zero) = Const 1
reduction (Sin Zero) = Zero
reduction (Cos Zero) = Const 1

reduction (Sum (Const x) (Const y)) = Const (x + y)
reduction (Diff (Const x) (Const y)) = Const (x - y)
reduction (Mult (Const x) (Const y)) = Const (x * y)
reduction (Sum (Mult f X) (Mult g X)) = Mult (Sum f g) X
reduction a = a