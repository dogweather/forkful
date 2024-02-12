---
title:                "Виведення налагоджувальної інформації"
date:                  2024-01-20T17:52:44.562989-07:00
model:                 gpt-4-1106-preview
simple_title:         "Виведення налагоджувальної інформації"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why?
"Що та Навіщо?"
Debug printing means showing variable values, error messages, or trace steps in your console. Programmers do it to understand what's happening in their code during runtime.

## How to:
"Як це зробити:"
Print a simple message to the console:

```Javascript
console.log("Hello, debug world!");
```
Output:

```
Hello, debug world!
```

Show a variable's value:

```Javascript
let someValue = 42;
console.log("The value is:", someValue);
```

Output:

```
The value is: 42
```

Catch and display an error:

```Javascript
try {
  throw new Error("Oops, an error occurred!");
} catch (error) {
  console.error("Caught an error:", error);
}
```

Output:

```
Caught an error: Error: Oops, an error occurred!
```

## Deep Dive
"Поглиблений Занурення"

Historical context: `console.log` has been a part of JavaScript for ages. Formerly, programmers used alert dialogs (`alert()`) or document writing (`document.write()`) for debug output, which were intrusive and limited.

Alternatives: Apart from `console.log`, there are `console.info`, `console.warn`, and `console.error` for different levels of importance. Tools like debuggers, profilers, and IDEs often provide more advanced debug capabilities.

Implementation details: The `console` object's methods work with web browsers and Node.js. They can handle multiple arguments, format them, and even display complexity like objects and stack traces.

## See Also
"Дивіться Також"

- MDN Web Docs on `console`: https://developer.mozilla.org/en-US/docs/Web/API/console
- Node.js documentation on `console`: https://nodejs.org/api/console.html
- An overview of JavaScript debugging with Chrome DevTools: https://developers.google.com/web/tools/chrome-devtools/javascript
