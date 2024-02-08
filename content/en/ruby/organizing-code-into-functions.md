---
title:                "Organizing code into functions"
aliases:
- en/ruby/organizing-code-into-functions.md
date:                  2024-01-25T02:59:47.815861-07:00
model:                 gpt-4-1106-preview
simple_title:         "Organizing code into functions"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## What & Why?
Organizing code into functions splits your script into reusable chunks. It's all about making your code clean, manageable, and less buggy. Modular code rocks because it saves you time, keeps your sanity, and simplifies debugging and unit testing.

## How to:
Imagine you're writing a quick script to greet users:

```Ruby
def greet(name)
  "Hello, #{name}!"
end

puts greet("Alice")   # Output: Hello, Alice!
puts greet("Bob")     # Output: Hello, Bob!
```

Or maybe you're calculating the area of a circle:

```Ruby
def circle_area(radius)
  Math::PI * radius ** 2
end

puts circle_area(5)   # Output: 78.53981633974483
```

Neater and easier to handle, right?

## Deep Dive
The concept of functions, also known as methods in Ruby, isn't new—it's as old as programming itself. Circling back to the 1950s, subroutines, as they were known, were introduced to reduce redundancy.

Alternatives? Sure, you've got inline code, you could go OOP with classes and objects, or even functional with lambdas and procs. But functions are the bread and butter of orderly code. Want performance? Local variables in functions are fast and functions can return values immediately with `return`.

Implementation-wise, you can define a function with `def` and end it with `end`. You can set default parameters, use splat operators for variadic functions, and more. Functions can be as simple or complex as your heart desires.

## See Also
- [Ruby's method documentation](https://ruby-doc.org/core-2.7.0/Method.html)
- [Learn to Program by Chris Pine](https://pine.fm/LearnToProgram/)
- [Practical Object-Oriented Design in Ruby by Sandi Metz](https://www.poodr.com/)
