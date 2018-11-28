# Problem
In this assignment, you will write a type-checker for a simple functional language.

You need to write a Prolog predicate hastype(Gamma, E, T), where 

• Gamma is a list of variable-type pairs, representing type assumptions on variables
• E is an object language expression, 
• T is a type.

This predicate is mutually recursively defined with another Prolog predicate

typeElaborates(Gamma, D, Gamma') where D is a definition.

*E ranges over (at least)*

• variables, modelled as say variable(X)<br/>
• constants, both numerical and boolean (at least)<br/>
• arithmetic operations over numerical expressions<br/>
• boolean operations over boolean expressions<br/>
• comparison operations over numerical expressions<br/>
• equality over arbitrary expressions, where equality can be decided<br/>
• conditional expressions if_then_else<br/>
• qualified expressions of the form let D in E end<br/>
• function abstractions \X.E  with functions as first-class citizens<br/>
• function application (E1 E2)  <br/>
• n-tuples  (n >= 0)<br/>
• expressions using projection operations.<br/>
• ....possible extensions to constructors, and case analysis expressions<br/>

*D ranges over (at least)*

• simple definitions X =def= E<br/>
• sequential definitions D1; D2<br/>
• parallel definitions D1 || D2<br/>
• local definitions local D1 in D2 end<br/>
• ... possible extension to recursive definitions<br/>

*T ranges over (at least)*

• Type variables modelled as say TypeVar(A) <br/>
• Base types intT, boolT, ...<br/>
• Arrow types T1 -> T2 |<br/>
• cartesian product types T1 * ... * Tn  (n>1)<br/>
• ... possible extension to union types and recursive types...<br/>

You will need to define suitable constructors for expressions, definitions, types, etc. You need to provide enough test examples to show your type checker works correctly.<br/>

## Note
This checker can work as a type inference engine.  However it does not work for polymorphic type inference.  Show with counter-examples that this is the case.
