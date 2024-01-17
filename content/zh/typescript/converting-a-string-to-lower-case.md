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

## 什么 & 为什么？
将字符串转换成小写是一种常见的编程技术，它允许程序员将字符串中的所有字母都转换成小写形式。程序员通常这样做是因为小写字符串可以更容易地进行比较，从而简化代码的编写和阅读。

## 如何：
在 TypeScript 中，要将一个字符串转换成小写形式，可以使用内置函数 `toLowerCase()` 。下面是一个示例代码和输出，展示了该函数的使用方法。

```typescript
let str = "HeLLo WoRld";
let lowerStr = str.toLowerCase();
console.log(lowerStr); // 输出：hello world
```

在上面的代码中，我们首先声明了一个包含混合大小写字母的字符串 `str` ，然后使用 `toLowerCase()` 函数将其转换成小写形式，并将结果赋值给变量 `lowerStr` ，最后通过 `console.log()` 函数打印出结果。

## 深入探讨：
历史上，计算机软件通常只能处理大写字符，因此在编程中，将字符串转换成小写形式是必要的。但随着技术的发展，现代计算机可以处理小写字符，因此这种转换已不再是必须的。作为替代方案，程序员也可以使用 `toUpperCase()` 函数将字符串转换成大写形式。

在实现上，`toLowerCase()` 函数会遍历字符串的每个字符，并将所有大写字母转换成小写字母。这个过程是在运行时完成的，因此可能会对系统性能产生微小的影响，尤其是在大型字符串上。因此，在处理大量字符串时，最好谨慎使用该函数。

## 参考链接：
- [官方文档：toLowerCase()](https://www.typescriptlang.org/docs/handbook)