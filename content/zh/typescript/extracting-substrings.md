---
title:                "TypeScript: 提取子字符串"
simple_title:         "提取子字符串"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## 为什么

为什么会有人对提取子字符串感兴趣呢？从编程的角度来说，提取子字符串可以帮助我们更方便地处理字符串，例如搜索、替换和格式化等操作。提取子字符串也可以帮助我们更有效地处理大量数据，提高程序的性能。

## 如何进行提取子字符串

我们可以使用 TypeScript 中的 `substring()` 方法来提取子字符串。这个方法接收两个参数，第一个参数是开始提取的索引位置，第二个参数是子字符串的长度。

```TypeScript
const str = '你好，世界！';
const subStr = str.substring(2, 3);
console.log(subStr); // 输出：好
```

此外，我们也可以使用 `slice()` 方法来提取子字符串，它的用法和 `substring()` 类似。同时，我们还可以使用 `substr()` 方法来提取子字符串，它接收两个参数，第一个参数是开始提取的索引位置，第二个参数是子字符串的结束索引位置。

```TypeScript
const str = 'Hello, world!';
const subStr = str.slice(2, 4);
console.log(subStr); // 输出：ll
const subStr2 = str.substr(2, 4);
console.log(subStr2); // 输出：llo,
```

## 深入了解提取子字符串

了解提取子字符串的详细信息可以帮助我们更灵活地运用它们。当我们提供的索引位置为负数时，`substring()` 和 `slice()` 方法会从字符串末尾开始计算。而 `substr()` 方法则会将负数索引位置转换为字符串的长度加上索引位置，然后再开始提取。此外，如果只提供一个参数，`substring()` 和 `slice()` 方法会从提供的索引位置一直提取到字符串末尾，而 `substr()` 方法会提取后面的所有字符。

## 参考链接

- [MDN 文档：substring()](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [MDN 文档：slice()](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
- [MDN 文档：substr()](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/String/substr)

## 参见