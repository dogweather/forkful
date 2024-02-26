---
date: 2024-01-25 03:00:08.560012-07:00
description: "Rounding numbers means adjusting them to the nearest integer or specified\
  \ decimal place. It's a staple in programming for reducing complexity, enhancing\u2026"
lastmod: '2024-02-25T18:49:56.639999-07:00'
model: gpt-4-1106-preview
summary: "Rounding numbers means adjusting them to the nearest integer or specified\
  \ decimal place. It's a staple in programming for reducing complexity, enhancing\u2026"
title: Rounding numbers
---

{{< edit_this_page >}}

## What & Why?
Rounding numbers means adjusting them to the nearest integer or specified decimal place. It's a staple in programming for reducing complexity, enhancing performance, and for times when precision beyond a certain point doesn't add value.

## How to:
```lua
-- Basic rounding in Lua doesn't come built-in, but you can define a function:

function round(num)
    return num >= 0 and math.floor(num + 0.5) or math.ceil(num - 0.5)
end

print(round(3.5))  -- 4
print(round(2.3))  -- 2
print(round(-1.6)) -- -2

-- To round to a specific decimal place:
function round(num, decimalPlaces)
    local mult = 10^(decimalPlaces or 0)
    return math.floor(num * mult + 0.5) / mult
end

print(round(3.14159, 2)) -- 3.14
print(round(1.98765, 3))  -- 1.988
```

## Deep Dive
Lua doesn't include a round function out of the box unlike some other languages. Historically, you need to write your own or use a third-party library. Common workarounds rely on `math.floor()` for rounding down and `math.ceil()` for rounding up, coupled with adding or subtracting 0.5 before doing so, depending on the number's sign.

Alternatives to rolling your own function include libraries such as "lua-users wiki" or "Penlight". Each has its benefits and trade-offs, like additional features or more overhead.

Internally, these functions normally work by exploiting the way computers store floating-point numbers. Adding 0.5 to a positive float that you want to round will push it over the threshold of the next integer value, so when you apply `math.floor()` it rounds down to that nearest integer.

## See Also
- [Lua 5.4 Reference Manual: The Mathematical Functions](https://www.lua.org/manual/5.4/manual.html#6.7)
- [Penlight Lua Libraries: Math](https://github.com/lunarmodules/Penlight)
