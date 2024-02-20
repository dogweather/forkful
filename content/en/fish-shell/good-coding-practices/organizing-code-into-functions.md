---
changelog:
- 2024-01-28, dogweather, reviewed and added links
date: 2024-01-25 03:00:10.607473-07:00
description: "Organizing code into functions is about bundling up bits of script to\
  \ do specific tasks. We do it because it makes code easier to read, test, and reuse\
  \ \u2014\u2026"
lastmod: 2024-02-19 22:05:18.939760
model: gpt-4-1106-preview
summary: "Organizing code into functions is about bundling up bits of script to do\
  \ specific tasks. We do it because it makes code easier to read, test, and reuse\
  \ \u2014\u2026"
title: Organizing code into functions
---

{{< edit_this_page >}}

## What & Why?
Organizing code into functions is about bundling up bits of script to do specific tasks. We do it because it makes code easier to read, test, and reuse — no one wants to wade through a swamp of code spaghetti.

## How to:
In Fish, you write a function with `function` keyword, give it a name, and end with `end`. Here’s a simple one:

```fish
function hello
    echo "Hello, World!"
end

hello
```

Output:
```
Hello, World!
```

Now, let's make it greet a user:

```fish
function greet
    set user (whoami)
    echo "Hey there, $user!"
end

greet
```

Output:
```
Hey there, your_username!
```

To save it across sessions, use `funcsave greet`.

## Deep Dive
Fish Shell functions are like mini-scripts — you can shove pretty much anything in there. Historically, the concept of functions in shell scripting has saved countless hours of repetitive typing and debugging. Unlike programming languages like Python, Shell functions are more about convenience than structure.

Some shells, like Bash, use `function` or just straight braces. Fish sticks to `function ... end`— clear and readable. Inside Fish functions, you get all the bells and whistles: parameters, local variables with `set -l`, and you can even define a function inside another function.

You won't need a `return` value because Fish isn't big on that; your function's output is its return. And if you want persistent functions available for future sessions, remember `funcsave`.

## See Also

- The fish tutorial on functions: [https://fishshell.com/docs/current/tutorial.html#tut_functions](https://fishshell.com/docs/current/tutorial.html#functions)

### Function commands

- [function](https://fishshell.com/docs/current/cmds/function.html) — Create a function
- [functions](https://fishshell.com/docs/current/cmds/functions.html) — Print or erase functions
- [funcsave](https://fishshell.com/docs/current/cmds/funcsave.html) — Save the definition of a function to the user’s autoload directory
- [funced](https://fishshell.com/docs/current/cmds/funced.html) — Edit a function interactively
