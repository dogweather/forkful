---
title:                "提取子字符串"
date:                  2024-01-20T17:46:13.180319-07:00
model:                 gpt-4-1106-preview
simple_title:         "提取子字符串"

category:             "Javascript"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? 什么和为什么？
提取子字符串就是从一个字符串中获取一部分内容。程序员这样做是为了处理文本数据，比如从用户输入中获取关键信息，或者为字符串分析做准备。

## How to: 如何操作
```javascript
// 使用 substring 方法
let text = "Hello, World!";
let subtext = text.substring(7, 12);
console.log(subtext); // 输出 "World"

// 使用 slice 方法
let slicedText = text.slice(7, 13);
console.log(slicedText); // 输出 "World!"

// 使用 substr 方法 (已废弃，请慎用)
let subTextDeprecated = text.substr(7, 5);
console.log(subTextDeprecated); // 输出 "World"
```

## Deep Dive 深入探究
提取子字符串的方法有很多年历史了，它们在JavaScript的早期版本就已经存在。`substring` 和 `slice` 是最常用的方法。`substr` 方法也可以用，但已经被弃用，未来的JavaScript版本中可能会移除。

`substring` 和 `slice` 的区别在于，`substring` 对负参数不敏感（它会将负数参数视为 `0`），而 `slice` 会将负数参数解释为字符串末尾的偏移量。它们在处理起止参数时也有差异。

要注意的是 `substring` 和 `slice` 方法不会改变原字符串，而是返回一个新的字符串。

## See Also 相关资源
- [MDN 文档 - String.prototype.substring()](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [MDN 文档 - String.prototype.slice()](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
- [ECMAScript 规范](https://www.ecma-international.org/ecma-262/)
