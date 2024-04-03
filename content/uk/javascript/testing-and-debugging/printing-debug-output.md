---
date: 2024-01-20 17:52:44.562989-07:00
description: "How to: \"\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\
  \u0438:\" Print a simple message to the console."
lastmod: '2024-03-13T22:44:49.997641-06:00'
model: gpt-4-1106-preview
summary: "\"\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438:\"\
  \nPrint a simple message to the console."
title: "\u0412\u0438\u0432\u0435\u0434\u0435\u043D\u043D\u044F \u043D\u0430\u043B\u0430\
  \u0433\u043E\u0434\u0436\u0443\u0432\u0430\u043B\u044C\u043D\u043E\u0457 \u0456\u043D\
  \u0444\u043E\u0440\u043C\u0430\u0446\u0456\u0457"
weight: 33
---

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
