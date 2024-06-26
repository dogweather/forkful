---
date: 2024-01-25 03:00:04.589438-07:00
description: 'How to: Elm doesn''t have built-in complex number support, so you''ll
  create your own type and functions. Here''s a quick setup.'
lastmod: '2024-03-13T22:45:00.003815-06:00'
model: gpt-4-1106-preview
summary: Elm doesn't have built-in complex number support, so you'll create your own
  type and functions.
title: Working with complex numbers
weight: 14
---

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
