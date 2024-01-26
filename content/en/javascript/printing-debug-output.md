---
title:                "Printing debug output"
date:                  2024-01-20T17:53:03.734143-07:00
model:                 gpt-4-1106-preview
simple_title:         "Printing debug output"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why?

Printing debug output in JavaScript is about showing variables, errors, or any other info that helps figure out what your code is doing at a glance. Programmers do it to catch bugs, understand the execution flow, and make sure the code is doing what it's supposed to do.

## How to:

Javascript makes it super easy to print debug output using `console.log()`. Here's how:

```javascript
console.log('Hello, debug world!');

let number = 42;
console.log('The number is:', number);

function add(a, b) {
  console.log(`Adding ${a} + ${b}`);
  return a + b;
}

let result = add(3, 4);
console.log('Result:', result);
```

Sample output in your browser's console or Node.js terminal would look like this:

```
Hello, debug world!
The number is: 42
Adding 3 + 4
Result: 7
```

## Deep Dive

The `console.log()` method comes from the Console API, which has been a debugging friend in browsers and Node.js environments for ages. But there's more than just `log`; you've got `console.warn()`, `console.error()`, and `console.info()`, all spitting out messages with different levels of severity.

Long ago, developers would use `alert()` for debugging, but that quickly became tedious—it blocks user interaction by popping up a dialog box.

There's also `console.dir()` which gives you a JSON-like view of an object, handy for deep inspection. If you want to track how long something takes, `console.time()` and `console.timeEnd()` are your pals.

For those loving a good, clean output, `console.table()` displays data in a neat table format. And when you go beyond simple debugging and step into performance land, the Console API has even more tools like `console.trace()` for call stack info, `console.profile()` for performance profiling, among others.

The exact way `console` methods are implemented can vary between JavaScript environments, but the essence remains the same: they help developers make sense of what's going under the hood quickly and with minimal fuss.

## See Also

- MDN Web Docs on Console API: https://developer.mozilla.org/en-US/docs/Web/API/Console
- Node.js `console` documentation: https://nodejs.org/api/console.html
- A guide to console commands: https://getfirebug.com/wiki/index.php/Console_API
