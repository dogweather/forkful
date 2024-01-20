---
title:                "Printing debug output"
html_title:           "Arduino recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why?

Debug output is a temporary print statement programmers use to understand what's happening inside their code or uncover bugs. It's crucial for developers to debug their program for a seamless and bug-free user experience.

## How to:
For a basic setup, use the built-in `console.log()` method in TypeScript:

```TypeScript
const message:string = "Hello, world!";
console.log(message);
```

Running this code should display your debug output:

```Shell
Hello, world!
```

For more complex programs, you might want to log objects or arrays:

```TypeScript
const complexObj = {
  name: "John",
  age: 25,
  favoriteFoods: ['pizza', 'pasta']
};

console.log(complexObj);
```

Output would be the object:

```Shell
{ name: 'John', age: 25, favoriteFoods: [ 'pizza', 'pasta' ] }
```

## Deep Dive

Historically, developers didn't have sophisticated debug tools, so they relied on print statements to troubleshoot the code's behavior, a practice that has carried over to modern coding.

Instead of console.log, you could use alternatives, such as using a debugger like Visual Studio Code's JavaScript debugger, or online services like StackBlitz or CodeSandbox for a more interactive debugging process.

Important to note, when utilizing console.log for debug purposes, remember to eliminate these statements from production code. It's a good practice to centralize logging and toggle it on or off based on the development environment.

## See Also

- JavaScript Debugging (an essential skill for TypeScript developers): https://developer.mozilla.org/en-US/docs/Learn/JavaScript/First_steps/What_went_wrong
- VS Code's debug tool: https://code.visualstudio.com/Docs/editor/debugging
- Interactive debugging with CodeSandbox: https://codesandbox.io/docs/debugging