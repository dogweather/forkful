---
title:                "找到字符串的长度"
html_title:           "TypeScript: 找到字符串的长度"
simple_title:         "找到字符串的长度"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 为什么

字符串是编程中经常使用的数据类型，而知道字符串的长度对于操作字符串或者数据处理来说非常重要。在TypeScript中，我们可以很方便地获得字符串的长度，并且这个操作也是相当实用的。

## 如何

要计算字符串的长度，我们可以使用内置的length属性。让我们来看一个例子：

```TypeScript
let str: string = "Hello World";
console.log(str.length);
```

输出结果为 `11`，这就是 `Hello World` 字符串的长度。同时，length属性也支持多字节字符，这意味着它对于处理Unicode字符也是有效的。

```TypeScript
let str: string = "你好世界";
console.log(str.length);
```

输出结果为 `4`，尽管这个字符串有6个字符，但是由于是双字节字符，所以长度仍然是4。

## 深入探讨

在JavaScript中，length属性实际上是一个方法而不是属性，它会通过遍历字符串来计算长度。而在TypeScript中，length是一个getter属性，它会在编译时被转换成了方法。这样做的好处是可以提高性能，因为我们可以在编译时就知道字符串的长度，而不需要在运行时去计算。

此外，length属性也适用于其他数据类型，比如数组和元组。它可以帮助我们快速获得数据的长度，而不需要额外的计算。

## 看看这个

- [TypeScript官方文档-字符串](https://www.typescriptlang.org/docs/handbook/strings.html)
- [ES6中的字符串长度计算方法](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [Unicode与UTF-8编码](https://www.ruanyifeng.com/blog/2007/10/ascii_unicode_and_utf-8.html)

## 参考链接

- [TypeScript Handbook](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)