---
date: 2024-01-20 17:46:13.180319-07:00
description: "How to: \u5982\u4F55\u64CD\u4F5C \u63D0\u53D6\u5B50\u5B57\u7B26\u4E32\
  \u7684\u65B9\u6CD5\u6709\u5F88\u591A\u5E74\u5386\u53F2\u4E86\uFF0C\u5B83\u4EEC\u5728\
  JavaScript\u7684\u65E9\u671F\u7248\u672C\u5C31\u5DF2\u7ECF\u5B58\u5728\u3002`substring`\
  \ \u548C `slice` \u662F\u6700\u5E38\u7528\u7684\u65B9\u6CD5\u3002`substr` \u65B9\
  \u6CD5\u4E5F\u53EF\u4EE5\u7528\uFF0C\u4F46\u5DF2\u7ECF\u88AB\u5F03\u7528\uFF0C\u672A\
  \u6765\u7684JavaScript\u7248\u672C\u4E2D\u53EF\u80FD\u4F1A\u79FB\u9664\u3002 `substring`\
  \ \u548C `slice`\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:48.481384-06:00'
model: gpt-4-1106-preview
summary: "\u5982\u4F55\u64CD\u4F5C \u63D0\u53D6\u5B50\u5B57\u7B26\u4E32\u7684\u65B9\
  \u6CD5\u6709\u5F88\u591A\u5E74\u5386\u53F2\u4E86\uFF0C\u5B83\u4EEC\u5728JavaScript\u7684\
  \u65E9\u671F\u7248\u672C\u5C31\u5DF2\u7ECF\u5B58\u5728\u3002`substring` \u548C `slice`\
  \ \u662F\u6700\u5E38\u7528\u7684\u65B9\u6CD5\u3002`substr` \u65B9\u6CD5\u4E5F\u53EF\
  \u4EE5\u7528\uFF0C\u4F46\u5DF2\u7ECF\u88AB\u5F03\u7528\uFF0C\u672A\u6765\u7684JavaScript\u7248\
  \u672C\u4E2D\u53EF\u80FD\u4F1A\u79FB\u9664."
title: "\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32"
weight: 6
---

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
