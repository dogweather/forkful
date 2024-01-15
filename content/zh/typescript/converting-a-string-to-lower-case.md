---
title:                "将字符串转换为小写"
html_title:           "TypeScript: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 为什么要将字符串转换为小写

对于许多编程任务来说，字符串是一个重要的数据类型。有时候，我们需要对字符串进行格式化和比较，其中一个常见的需求就是将所有的字符转换为小写形式。这篇文章将向您展示如何用 TypeScript 轻松地完成这项任务。

## 如何做到

```TypeScript
let originalStr = "Hello World!";
let lowerStr = originalStr.toLowerCase();

console.log(lowerStr); // output: hello world!
```

在这个例子中，我们定义了一个名为 `originalStr` 的字符串变量，并将其初始化为 `"Hello World!"`。然后，我们调用字符串的 `toLowerCase()` 方法，将 `originalStr` 中的所有字符转换为小写形式，并将结果储存在名为 `lowerStr` 的新变量中。最后，我们使用 `console.log()` 来打印 `lowerStr` 的值，以检查转换是否成功。

## 深入探索

若要深入了解字符串转换为小写的实现原理，我们首先需要了解 TypeScript 中的字符串是如何存储的。在 TypeScript 中，字符串是使用 `string` 类型来表示的，它是一种原始数据类型，也就是说它是不可变的，我们不能直接对其进行修改。因此，当我们调用 `toLowerCase()` 方法时，它并不会改变原始字符串的值，而是返回一个新的字符串，其中包含了转换后的结果。

此外，值得注意的是，`toLowerCase()` 方法只会将字母字符转换为小写形式，其他字符（比如数字、标点符号等）会保持不变。因此，这个方法适用于大多数情况下，但在某些特殊情况下，我们可能需要自定义转换方式来处理特殊字符。

## 参考链接

- [TypeScript 官方文档 - 字符串操作](https://www.typescriptlang.org/docs/handbook/strings.html)
- [MDN Web 文档 - toLowerCase() 方法](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)

## 参见

- [TypeScript 字符串操作指南](https://blog.logrocket.com/string-operations-in-typescript/)