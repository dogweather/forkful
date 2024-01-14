---
title:    "TypeScript: 将字符串转换为小写"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## 为什么 
为什么要将字符串转为小写? 将字符串转为小写有各种各样的用途。例如，当需要将用户输入的数据与另一个字符串进行比较时，为了避免大小写造成的错误，可以将输入的字符串转为小写来进行比较。

## 如何做
做这件事很简单，只需要使用 TypeScript 中的 `toLowerCase()` 方法即可。下面是一个示例代码和输出结果:

```TypeScript
const myString = "Hello World!";
console.log(myString.toLowerCase());
// 输出结果: hello world!
```

在上面的代码中，我们首先定义了一个字符串变量 `myString`，然后使用 `toLowerCase()` 方法将它转为小写，并将结果打印到控制台上。

## 深入探讨
那么，`toLowerCase()` 方法是如何将字符串转为小写的呢？它其实是通过 Unicode 编码来实现的。Unicode 是一种用来表示字符的标准，在 JavaScript 和 TypeScript 中都是使用 Unicode 来表示字符。每个字符都对应着一个唯一的 Unicode 编码，而在 ASCII 字符集中，字母 A 对应的编码是 65，而字母 a 对应的编码是 97。可以看出，它们的编码值之间相差了 32，所以当使用 `toLowerCase()` 方法将字符串转为小写时，实际上就是将字符串中的每个字符的 Unicode 编码值加上 32 来实现的。

## 看看这些
* [TypeScript 入门教程](https://www.typescriptlang.org/docs/handbook/typescript-in-5-minutes.html)
* [Unicode 编码表](https://unicode-table.com/zh/)