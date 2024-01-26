---
title:                "Rounding numbers"
date:                  2024-01-25T03:00:01.692945-07:00
model:                 gpt-4-1106-preview
simple_title:         "Rounding numbers"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/rounding-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Rounding numbers means adjusting them to the nearest whole number or to a specified degree of precision. Programmers round numbers to simplify, to match human expectations, or to fit data into specific formats—think financial calculations, graphical displays, or reducing storage size.

## How to:

```Ruby
# Basic rounding
puts 3.14159.round      # => 3
puts 2.6.round          # => 3

# Specifying precision
puts 3.14159.round(2)   # => 3.14
puts 2.675.round(2)     # => 2.68

# Rounding down
puts 2.9.floor          # => 2

# Rounding up
puts 2.1.ceil           # => 3

# Rounding towards zero
puts -2.9.round         # => -3
puts -2.9.truncate      # => -2
```

Sample Output:
```
3
3
3.14
2.68
2
3
-3
-2
```

## Deep Dive
Rounding numbers isn't new—humans have been doing it for centuries to make calculations easier or to work within the limits of their tools. In Ruby, the `round` method is versatile, with the ability to round to the nearest whole number by default or to a specified decimal place.

An alternative to `round` is `floor` for always rounding down, and `ceil` for always rounding up, regardless of the number's value. To just chop off the decimal places, you've got `truncate`.

Historically, when it comes to computers, rounding becomes critical in dealing with floating-point arithmetic due to its inherent imprecision. Ruby, like most languages, follows the IEEE 754 standard for floating-point numbers, which means it handles rounding in a way that most programmers should be able to predict and rely on.

There's more to it, though—things like the banker's rounding (also known as round half to even) are concepts that Ruby developers may need to manually implement, since the `round` method doesn't offer it out of the box.

## See Also
- The [Ruby Documentation](https://ruby-doc.org/core-3.0.0/Float.html#method-i-round) for Floats' `round` method.
- [IEEE Standard for Floating-Point Arithmetic (IEEE 754)](https://ieeexplore.ieee.org/document/4610935).
- [Understanding Floating-Point Precision](https://floating-point-gui.de/), for a deeper insight into how computers handle decimal numbers.
