---
date: 2024-01-20 17:46:13.180319-07:00
description: "\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32\u5C31\u662F\u4ECE\u4E00\u4E2A\u5B57\
  \u7B26\u4E32\u4E2D\u83B7\u53D6\u4E00\u90E8\u5206\u5185\u5BB9\u3002\u7A0B\u5E8F\u5458\
  \u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u5904\u7406\u6587\u672C\u6570\u636E\uFF0C\u6BD4\
  \u5982\u4ECE\u7528\u6237\u8F93\u5165\u4E2D\u83B7\u53D6\u5173\u952E\u4FE1\u606F\uFF0C\
  \u6216\u8005\u4E3A\u5B57\u7B26\u4E32\u5206\u6790\u505A\u51C6\u5907\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:48.193539-06:00'
model: gpt-4-1106-preview
summary: "\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32\u5C31\u662F\u4ECE\u4E00\u4E2A\u5B57\
  \u7B26\u4E32\u4E2D\u83B7\u53D6\u4E00\u90E8\u5206\u5185\u5BB9\u3002\u7A0B\u5E8F\u5458\
  \u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u5904\u7406\u6587\u672C\u6570\u636E\uFF0C\u6BD4\
  \u5982\u4ECE\u7528\u6237\u8F93\u5165\u4E2D\u83B7\u53D6\u5173\u952E\u4FE1\u606F\uFF0C\
  \u6216\u8005\u4E3A\u5B57\u7B26\u4E32\u5206\u6790\u505A\u51C6\u5907\u3002."
title: "\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32"
weight: 6
---

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
