# Problem Statement
Consider the data type **A*** of strings over a given alphabet **A**, which inductively defined as:<br/>

 • The empty string (which has no letters) is in **A***<br/>
 • Each letter a in **A** is in A*<br/>
 • If s_1 and s_2 are strings in A*, then their concatenation s_1 s_2 is in A*<br/>
 
Define in OCaml the following:<br/>
1. An efficient **functional data type** to represent **editable strings** (which can be arbitrarily long) over an **arbitrary** given alphabet.<br/>
2. A function lgh, which given a string over the given alphabet returns a non-negative integer which is the number of letters in the string.<br/>
3. A function nonempty that returns false if a given string is empty and true otherwise.<br/>
4. A function concat that concatenates two given strings  [ What is its complexity? Also prove that lgh( concat s1 s2) = lgh(s1) + lgh(s2). ]<br/>
5. A function reverse, which reverses the characters in a string   [ Its complexity should be O(lgh(s)).  Also prove that  lgh(reverse s) = lgh(s). ]  <br/>
6. A function first that returns the first letter of a given non-empty string, raising an exception Empty otherwise. [ This should be O(1). ]<br/>
7. A function last that returns the last letter of a given non-empty string, raising an exception Empty otherwise.  [ This should be O(1). ]<br/>
   <br/>{Edit functions, assuming that when creating a string,  the initial position of the marker will be 0. ]<br/>
8. A function create which given an OCaml string, creates an editable string, with the initial position of the edit marker being 0<br/>
9. A function forward that when a marker points to the kth position in the string moves it to the (k+1)-th position, if it exists, and raising AtLast otherwise. [ What is its complexity? We want it to be O(1).  By default, the initial position of the marker when creating a string  is 0. ]<br/>
10. A function back that when the marker points to the kth position in the string moves it to the (k-1)-th position, if it exists, and raising AtFirst otherwise. [ What is its complexity? We want it to be O(1). ]<br/>
11. A function moveTo that given a non-negative integer n and a string s, moves the marker to the nth letter of s, counting from 0.  If n >= lgh s, then raise exception TooShort.  [ What is its complexity? We want it to be O(n), where n is the given argument. ]<br/>
12. A function replace which (assuming the marker is at a position n>= 0) in a string s, and a letter w, replaces the letter at the n-th position of s with w.  [ Prove that lgh(replace w s) = lgh(s). ]<br/>
