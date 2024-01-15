---
title:                "写入标准错误"
html_title:           "Javascript: 写入标准错误"
simple_title:         "写入标准错误"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 为什么

写入标准错误 (Standard Error) 是一种常见的调试技术，可以帮助开发人员识别并解决 JavaScript 代码中的错误。它可以让开发人员快速定位错误并进行调试，提高代码质量。

## 如何操作

```javascript
console.error("Oops, something went wrong!"); // Output: Oops, something went wrong!
```

以上是一个简单的示例，演示了如何使用 `console.error()` 函数将错误信息输出到标准错误。在这个例子中，我们将错误信息设置为 "Oops, something went wrong!"，然后通过调用 `console.error()` 函数将其输出到标准错误中。这样一来，在我们执行代码时，就可以在控制台中看到错误信息，从而帮助我们识别和解决代码中的问题。

## 深入探讨

标准错误的概念可能并不那么容易理解，但它实际上是与标准输出 (standard output) 相对应的概念。标准输出通常用于输出程序的正常结果，而标准错误则用于输出错误信息。它们通常都会显示在控制台中，但是各自有特定的作用，而且在某些情况下，它们可能会显示在不同的地方。因此，当我们使用 `console.error()` 函数将错误信息输出到标准错误时，我们就可以很容易地区分正常输出和错误信息。

## 参考资料

- [标准错误 (Standard Error)](https://zh.wikipedia.org/wiki/%E6%A8%99%E6%BA%96%E9%8C%AF%E8%AA%A4)
- [console.error()](https://developer.mozilla.org/zh-CN/docs/Web/API/Console/error)