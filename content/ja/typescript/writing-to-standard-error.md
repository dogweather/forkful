---
title:                "標準エラーへの書き込み"
html_title:           "TypeScript: 標準エラーへの書き込み"
simple_title:         "標準エラーへの書き込み"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Waiatto & Naze?

Writing to standard error, or stderr for short, is the act of sending error messages to a specific output stream separate from the regular output stream. This is commonly used by programmers as it allows for clear distinction between regular output and error messages, making debugging and troubleshooting easier. 

## Hou to:

```TypeScript
console.error("This is an error message."); 
```
**Output:**
```
This is an error message.
```

```TypeScript
console.log("This is a regular message."); 
```
**Output:**
```
This is a regular message.
```

## Jushin:

In the past, programmers would print all messages, including errors, to the regular output stream, making it difficult to spot errors. The adoption of stderr as a separate stream has greatly improved the debugging process. Alternatives to using stderr include using a designated log file for errors or using a logging library that allows for customizable output streams. In TypeScript, errors can be logged using the built-in `console.error()` function.

## Sono hokanikikin:

- [TypeScript Official Documentation on `console.error()`](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-2.html#error-outputs-in-console)
- [Explanation of stderr vs stdout](https://stackoverflow.com/questions/32059191/stderr-and-stdout-explained)