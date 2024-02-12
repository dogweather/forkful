---
title:                "Working with complex numbers"
aliases:
- /en/elm/working-with-complex-numbers.md
date:                  2024-01-25T03:00:04.589438-07:00
model:                 gpt-4-1106-preview
simple_title:         "Working with complex numbers"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Complex numbers are a combo of real and imaginary numbers, like `a + bi` where `i` is the square root of -1. They're key in fields like engineering and physics to solve problems regular numbers can't touch.

## How to:
Elm doesn't have built-in complex number support, so you'll create your own type and functions. Here's a quick setup:

```Elm
type alias Complex =
    { real : Float, imaginary : Float }

add : Complex -> Complex -> Complex
add a b =
    { real = a.real + b.real, imaginary = a.imaginary + b.imaginary }

-- Example usage:
a = { real = 3, imaginary = 2 }
b = { real = 1, imaginary = -4 }

sum = add a b
-- sum is { real = 4.0, imaginary = -2.0 }
```

## Deep Dive
Historically, complex numbers weren't always accepted. They became a game-changer in the 16th century to solve cubic equations. Alternatives in other languages like Python offer built-in complex number support with operations right out of the box. Elm requires a DIY approach as you've seen. But you can make it as sophisticated as needed, building multiplication, division, and other ops, tuning performance issues.

## See Also
- Elm's Official Documentation: https://package.elm-lang.org/ for creating custom types and mastering Elm basics.
- Math history buffs could check out "An Imaginary Tale" by Paul J. Nahin for a complex numbers' journey through time.
- Dive into math-oriented programming challenges on Project Euler (https://projecteuler.net) to apply your complex number wizardry.
