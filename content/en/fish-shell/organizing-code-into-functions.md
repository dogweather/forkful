---
title:                "Organizing code into functions"
date:                  2024-01-25T03:00:10.607473-07:00
model:                 gpt-4-1106-preview
simple_title:         "Organizing code into functions"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/organizing-code-into-functions.md"
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
- The fish tutorial on functions: https://fishshell.com/docs/current/tutorial.html#tut_functions
- The fish documentation for `function`: https://fishshell.com/docs/current/cmds/function.html
- An extensive guide on writing functions in fish: https://fishshell.com/docs/current/index.html#syntax-function
