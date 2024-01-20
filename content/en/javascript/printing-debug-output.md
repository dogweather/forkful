---
title:                "Printing debug output"
html_title:           "Arduino recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why?

Printing debug output in Javascript means using commands to display values and variables in the console. It's a vital tool in diagnosing and fixing issues in code.

## How to:

Here's the most common way to print debug output – the `console.log()` method.

```Javascript
console.log("Hello, World!");
var name = 'John';
console.log("Hello, " + name);
```

When you run this, the output in the console would be:

```
Hello, World!
Hello, John
```

You can also use `console.warn()` and `console.error()`, similar to `console.log()`, but they display messages as warnings and errors, respectively.

```Javascript
console.warn("This is a warning!");
console.error("This is an error message!");
```

Output:

```
Warning: This is a warning!
Error: This is an error message!
```

## Deep Dive

Historically, debug printing has roots in low-level languages. Initially, developers would display variable values directly on hardware panels. Nowadays, it's integral to practically all high-level languages, including Javascript.

There are alternatives to `console.log()`, such as `console.debug()`, `console.info()`, and `console.trace()`. While `console.log()` is the most popular, these offer different levels of verbose output and tracing.

Note that `console.log()` doesn't work in all JavaScript environments. For instance, in older versions of Internet Explorer, unless the developer tools are open, a `console.log()` would throw an error.

## See Also

For further exploration of Javascript's `console` object and its methods, check out these resources:

1. [Mozilla Developer Network (MDN) Web Docs Console](https://developer.mozilla.org/en-US/docs/Web/API/console)
2. [JavaScript.info console tutorial](https://javascript.info/debugging-chrome)
3. [Google Developers Console API Reference](https://developers.google.com/web/tools/chrome-devtools/console/api)