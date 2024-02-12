---
title:                "Using an interactive shell (REPL)"
aliases: - /en/javascript/using-an-interactive-shell-repl.md
date:                  2024-01-25T03:39:46.320314-07:00
model:                 gpt-4-1106-preview
simple_title:         "Using an interactive shell (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## What & Why?
Interactive shells, or REPLs (Read-Eval-Print Loops), let you run code on the fly, testing functions, algorithms, or fiddling with ideas. They're the scratch pads of coding, quick and dirty, without setting up a full dev environment.

## How to:
Node.js ships with a REPL accessible via the terminal. Pop it open, and you're ready to roll. Here's a taste:

```javascript
$ node
> let sum = (a, b) => a + b;
undefined
> sum(5, 10);
15
> .exit
```

Straightforward, right? Define variables, functions, or run loops. When done, `.exit` takes you back to the real world.

## Deep Dive
REPLs have been around since the 1960s – LISP pioneered the concept. The idea: give immediate feedback to the programmer. Alternatives? Besides Node.js REPL, there's browser-based consoles like Chrome DevTools, online sandboxes like JSFiddle, or full IDEs like VSCode with interactive playgrounds.

Under the hood, REPL workflows typically: 
1. Read input
2. Compile and execute code
3. Print output
4. Loop back

It's a simple yet effective cycle that has massively influenced interactive coding.

## See Also
- [Node.js REPL documentation](https://nodejs.org/api/repl.html)
- [Mozilla's Introduction to JavaScript modules on REPLs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Modules)
- [JSFiddle](https://jsfiddle.net/)
