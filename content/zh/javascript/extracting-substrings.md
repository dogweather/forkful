---
title:                "Javascript: 提取子串"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## 为什么要提取子字符串

在编程过程中，经常需要从一个较长的字符串中提取出想要的一部分内容。这种操作被称为提取子字符串。提取子字符串可以让我们更有效地处理字符串，从而提高程序的运行效率和性能。

## 如何实现提取子字符串

在Javascript中，提取子字符串可以通过使用 `substring()` 方法来实现。该方法接收两个参数，分别是起始位置和结束位置，可以提取出从起始位置到结束位置之间的子字符串。

例如，在下面的代码中，我们将使用 `substring()` 方法从一个字符串中提取出部分内容，并打印出来。

```Javascript
const str = "这是一个长长的字符串";
const subStr = str.substring(4, 8);
console.log(subStr); // 输出结果为 "长长的"
```

## 深入了解提取子字符串

除了 `substring()` 方法，Javascript还提供了其他几种提取子字符串的方法，比如 `slice()`、`substr()`和 `charAt()`等。每种方法都有其独特的功能，可以根据具体的需求来选择使用。

提取子字符串的操作涉及到对字符串的索引和长度的理解，所以在实现过程中，我们需要注意起始位置和结束位置的取值范围，以及子字符串的长度是否和预期一致。

## 参考资料

- [MDN - substring()](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [菜鸟教程 - JavaScript 子字符串提取函数](https://www.runoob.com/jsref/jsref-substring.html)
- [猿人学 - Javascript 字符串的操作——substr、substring](https://www.yuanrenxue.com/javascript-substr-substring.html)

## 参见

- [MDN - slice()](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
- [MDN - substr()](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/String/substr)
- [MDN - charAt()](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/String/charAt)