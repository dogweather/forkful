---
title:                "TypeScript: 寻找字符串的长度"
simple_title:         "寻找字符串的长度"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 为什么要找出字符串的长度？

计算字符串的长度是编程中必不可少的一部分。它可以帮助我们了解字符串的结构，以及在其他操作中使用字符串有多少字符。通过找出字符串的长度，我们可以更有效地处理字符串，提高我们的代码性能。

## 如何做

```TypeScript
// 定义一个字符串
let name: string = "张三";

// 使用.length属性找出字符串的长度
let length: number = name.length;

// 输出计算结果
console.log(length); // 2
```

在上面的示例中，我们首先定义了一个字符串变量`name`，它的值为`张三`。然后，我们使用`.length`属性来找出该字符串的长度，并将结果赋值给变量`length`。最后，使用`console.log()`函数将`length`打印出来，结果为`2`。这表明字符串`张三`由两个字符组成，即`张`和`三`。

## 深入探讨

要计算字符串的长度，我们需要了解计算机是如何处理字符串的。在计算机中，字符串是由一系列字符组成的。每个字符都有自己的编码，用来表示它在计算机中的存储方式。当我们使用`.length`属性时，计算机会逐个计算字符串中的字符数量，并将最终结果返回给我们。

但需要注意的是，`.length`属性返回的是字符串中字符的数量，而不是实际的字节数。因为不同的编码方式会影响到字符所占用的字节数。比如，英文字符在UTF-8编码中会占用1个字节，而中文字符则会占用3个字节。因此，一个包含英文和中文字符的字符串在计算长度时，返回的结果可能会不同。

## 参考链接

- [TypeScript官方文档](https://www.typescriptlang.org/)
- [字符串.length属性](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [Unicode编码](https://zh.wikipedia.org/wiki/Unicode)