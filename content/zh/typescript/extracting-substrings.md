---
title:                "TypeScript: 提取子字符串"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## 为什么要提取子字符串？

在编程方面，提取子字符串是一项常用的技术。它可以帮助我们更有效地处理文本数据，从而提高程序的性能和可读性。而在 TypeScript 中，提取子字符串也是一项轻而易举的任务。下面就让我们来看看如何实现，以及一些有用的技巧和深层知识。

## 如何提取子字符串

提取子字符串的一种常见方法是使用`substring()`方法。它可以从一个字符串中提取指定位置的子字符串。下面是一个基本的语法示例：

```TypeScript
let str = 'Hello World';
// 从索引3（包括）开始提取子字符串，到索引7（不包括）结束
let substr = str.substring(3, 7);
console.log(substr); // 输出结果： lo W
```

我们也可以使用负数的索引来表示从字符串末尾开始计数，例如：

```TypeScript
let str = 'Hello World';
// 从倒数第2个字符开始提取子字符串，到倒数第3个字符结束
let substr = str.substring(-2, -5);
console.log(substr); // 输出结果： ld
```

另一种常用的方法是使用`slice()`方法，它也可以从一个字符串中提取子字符串，但它允许我们使用负数索引来表示从字符串末尾开始计数。见下面的示例：

```TypeScript
let str = 'Hello World';
// 从索引3（包括）开始提取子字符串，到索引7（不包括）结束
let substr = str.slice(3, 7);
console.log(substr); // 输出结果： lo W
```

我们还可以使用`substr()`方法来提取子字符串，它采用两个参数：提取的起始位置和提取的长度。例如：

```TypeScript
let str = 'Hello World';
// 从索引3开始提取长度为5的子字符串
let substr = str.substr(3, 5);
console.log(substr); // 输出结果：lo Wo
```

当然，这只是提取子字符串的一些基本用法，还有很多其他方法和技巧可以发现。接下来，让我们来深入了解一下提取子字符串的一些重要概念和特性。

## 深入了解提取子字符串

首先，我们需要理解的是`substring()`方法和`slice()`方法的第二个参数并不是字符串的末尾索引，而是提取子字符串的结束位置。因此，当我们提供的位置参数超出字符串的长度时，这两个方法都会自动将位置调整为字符串的末尾。而对于`substr()`方法来说，第二个参数表示提取的长度，因此超出字符串长度时将返回空字符串。

其次，我们需要注意的是，`substring()`方法和`slice()`方法都不会改变原始字符串，而是返回一个提取的子字符串。而`substr()`方法会修改原始字符串，删除提取的子字符串后返回剩余的字符串。因此，如果我们希望保留原始字符串，可以使用`slice()`或`substring()`方法。

最后，我们需要了解的是，除了上述方法，还有一些其他方法可以用来提取子字符串，例如`charAt()`方法、`split()`方法等。每种方法都有自己的特点和使用场景，我们可以根据具体情况来选择使用。

## 参考链接

- [TypeScript substring()方法文档](https://www.typescriptlang.org/docs/handbook/strings.html#substring)
- [TypeScript slice()方法文档](https://www.typescriptlang.org/docs/handbook/strings.html#slice)
- [TypeScript substr()方法文档](https://www.typescriptlang.org/docs/handbook/strings.html#substr)
- [JavaScript字符串方法参考](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/String