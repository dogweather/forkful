---
title:                "Generating random numbers"
date:                  2024-01-20T17:49:34.996882-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generating random numbers"
programming_language: "Lua"
category:             "Lua"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?

Generating random numbers is creating values that are unpredictably different each time a program runs. Programmers use them for everything from games (dice rolls, anyone?) to simulations, to cybersecurity (think passwords and encryption keys).

## How to:

Lua makes tossing the dice easy. Check this out:

```lua
math.randomseed(os.time()) -- Seeds the RNG with the current time

-- Get a random number between 1 and 10
local randomNumber = math.random(1,10)
print(randomNumber) -- Output: a number between 1 and 10, changes each run
```

Pro tip: Call `math.randomseed()` once at the start; re-seeding often isn't helpful, really.

## Deep Dive

Back then, randomness in computers was a tough nut. The machines were too exacting—too perfect for "random." Now, programs use algorithms called PRNGs (Pseudo-Random Number Generators). They mimic randomness using mathematical wizardry, but true randomness? Nope, still deterministic if you know the state.

Lua uses a PRNG, too. It's good enough for a game of digital craps, but don't bet the farm on it for hardcore crypto stuff.

Alternatives? Lua can be friends with other languages that offer stronger random functions or link with C libraries like OpenSSL for cryptographic-quality random numbers.

## See Also

Interested in diving deeper? Check out these:

- [Lua Reference Manual](https://www.lua.org/manual/5.4/) (Look for `math.random` and `math.randomseed`)
- [Online Lua Demo](http://www.lua.org/demo.html) (Try out Lua in your browser)
- [Wikipedia on Random Number Generation](https://en.wikipedia.org/wiki/Random_number_generation) (For the history buffs and curious cats)
