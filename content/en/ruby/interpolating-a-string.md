---
title:                "Interpolating a string"
html_title:           "Arduino recipe: Interpolating a string"
simple_title:         "Interpolating a string"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? 

Interpolation is a method of injecting values directly into a string. It makes it easy to insert variables or computed values into a text in Ruby — saving programmers from fumbling with string concatenation.

## How to

String interpolation in Ruby looks like this:

```Ruby 
name = "Ruby"
puts "Hello, #{name}!"
```

The output would be:

```
Hello, Ruby!
```

You can also inject code. Consider that we need to calculate a total price:

```Ruby
quantity = 10
price_per_item = 2.5
puts "Total price is #{quantity * price_per_item}"
```

And the output:

```
Total price is 25.0
```

## Deep Dive 

String interpolation was introduced in Ruby 1.9, replacing the older and more verbose `sprintf` formatting approach. 

An alternative to interpolation is concatenation, but beware, interpolation often reads more easily. Compare these two pieces of code:

```Ruby
puts "Hello, " + name + "!"  # Concatenation 
puts "Hello, #{name}!"       # Interpolation
```

When interpolating, Ruby executes the code inside the curly braces and converts the result to a string. It's handy, but keep in mind your string cannot perform heavy computations or complex object manipulations.

## See Also

Look up these related topics:

- [Ruby docs: String](https://ruby-doc.org/core-2.7.0/String.html)
- [Ruby docs: String interpolation](https://ruby-doc.org/core-2.7.0/doc/syntax/literals_rdoc.html#label-Strings)
- [Intro to Ruby: String Interpolation](http://ruby-for-beginners.rubymonstas.org/bonus/string_interpolation.html)
- [ThoughtCo: How to Use String Interpolation in Ruby](https://www.thoughtco.com/string-interpolation-in-ruby-2907742).