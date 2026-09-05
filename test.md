### Reduction example

For example, to reduce the following lambda expression, we must first understand what it means.

In the outermost level, the expression is the application of $\lambda x.\lambda y.(xy)$ to itself.
It follows the second type of lambda calculus expression discussed earlier, $\mathcal{M}\mathcal{N}$ where $\mathcal{M}\in \Lambda$ and $\mathcal{N}\in \Lambda$.
 In this context $\mathcal{M} = (\lambda x.\lambda y.(xy))$ and also $\mathcal{N} = (\lambda x.\lambda y.(xy))$.


When you start evaluating this expression, you might be tempted to automatically apply a $\beta$ reduction by itself:

$$
\begin{aligned}
(\lambda x.\lambda y.(xy))(\lambda x.\lambda y.(xy))&=_{\beta}\lambda y.((\lambda x.\lambda y.(xy))y)\\
&=_{\beta}\lambda y.\lambda y.(yy)
\end{aligned}
$$

### Another Header

But this reduction is actually incorrect because the although $x$ and $y$ appear on both lambda expressions, these variables don't have the same meaning.
The $x$ and $y$ variables inside the left lambda expression are **bound** inside this lambda expression.
The $x$ and $y$ variables outside the left lambda expression (inside the right lambda expression) are **free** in its context, therefore, even though they look the same, it is incorrect to interchange the two variables.

Two avoid confusion with similarly named variables, it is advisable to apply $\alpha$ equivalencies, to give variables different names.
This can be done by replacing the right abstractions' bound variables with $u$ and $v$.
Again, this alpha reduction doesn't change the meaning of the abstraction, it merely renames the bound variables.

$$
\begin{aligned}
p_1 \lor p_2 \lor \cdots \lor r & \\
q_1 \lor q_2 \lor \cdots \lor \neg r & \\
\hline
p_1 \lor p_2 \lor q_1 \lor q_2 \cdots \\
\end{aligned}
$$
