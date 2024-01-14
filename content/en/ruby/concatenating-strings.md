---
title:    "Ruby recipe: Concatenating strings"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Why

Concatenating strings, or combining multiple strings together, is a common task in programming. It allows us to create dynamic and customized output for our users, making our code more user-friendly and versatile.

## How To

### Syntax
To concatenate strings in Ruby, we use the `+` operator. For example:

```Ruby
str1 = "Hello "
str2 = "world!"
puts str1 + str2
```
Output:
```
Hello world!
```
In the above example, we are using the `+` operator to combine the two strings `str1` and `str2`. The result is a new string with both values concatenated together.

### Using Variables
We can also use variables in our concatenation to make our code more dynamic. For example:

```Ruby
name = "John"
greeting = "Hello, " + name + "!"
puts greeting
```
Output:
```
Hello, John!
```
In this example, we are using the `name` variable to personalize our greeting. This allows us to create customized output depending on the value of the variable.

### Combining Different Data Types
We can also concatenate different data types together. For example:

```Ruby
age = 35
message = "I am " + age.to_s + " years old."
puts message
```
Output:
```
I am 35 years old.
```
In this example, we are using type conversion by using `.to_s` to convert the integer value of `age` into a string, so it can be concatenated with the other strings.

## Deep Dive

When concatenating strings, it is important to be aware of the order in which the strings are combined. This is known as the left-to-right associativity. For example, in the following code:

```Ruby
str1 = "Blues"
str2 = "Clues"
puts "Blue's " + str1 + " and " + str2 + "!"
```
Output:
```
Blue's Blues and Clues!
```
In this example, the first string `str1` is concatenated with the second string `str2`, and then the resulting string is concatenated with the final string "and". It is important to be mindful of this when working with multiple strings to get the desired output.

Another important concept to keep in mind when concatenating strings is string interpolation. This allows us to insert the value of a variable directly into a string, without having to use the `+` operator. For example:

```Ruby
name = "Sarah"
puts "Hello, #{name}!"
```
Output:
```
Hello, Sarah!
```
Notice how we used `#{}` to interpolate the value of the `name` variable into the string. This can make our code more concise and readable when working with multiple strings.

## See Also

To learn more about string concatenation and other important string operations in Ruby, check out these resources:

* [Ruby Docs: String Class](https://ruby-doc.org/core-2.6/String.html)
* [Concatenation vs. Interpolation in Ruby](https://thoughtbot.com/blog/concatenation-vs-interpolation-in-ruby)
* [Ruby String Operations: Concatenation and Interpolation](https://www.sitepoint.com/rubys-string-operations/)