---
date: 2024-01-20 17:58:39.789831-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C\uFF1A) \u641C\u7D22\u548C\u66FF\u6362\
  \u6587\u672C\u7684\u9700\u6C42\u8BDE\u751F\u4E8E\u65E9\u671F\u7F16\u7A0B\u7684\u65E5\
  \u5B50\uFF0C\u5F53\u65F6\u901A\u8FC7\u6279\u5904\u7406\u547D\u4EE4\u548C\u57FA\u672C\
  \u7684\u7F16\u8F91\u5668\u5B9E\u73B0\u3002\u73B0\u4ECA\uFF0CJavaScript \u4E2D\u63D0\
  \u4F9B\u4E86 `.replace()` \u65B9\u6CD5\u7528\u4E8E\u6587\u672C\u66FF\u6362\u3002\
  \u8FD9\u4E2A\u65B9\u6CD5\u53EF\u63A5\u53D7\u5B57\u7B26\u4E32\u6216\u6B63\u5219\u8868\
  \u8FBE\u5F0F\u4F5C\u4E3A\u641C\u7D22\u53C2\u6570\uFF0C\u5B9E\u73B0\u7075\u6D3B\u7684\
  \u6587\u672C\u64CD\u4F5C\u3002\u9664\u4E86 `.replace()`, \u66FF\u4EE3\u65B9\u6CD5\
  \u5982\u4F7F\u7528\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:38:47.337932-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u64CD\u4F5C\uFF1A) \u641C\u7D22\u548C\u66FF\u6362\u6587\u672C\
  \u7684\u9700\u6C42\u8BDE\u751F\u4E8E\u65E9\u671F\u7F16\u7A0B\u7684\u65E5\u5B50\uFF0C\
  \u5F53\u65F6\u901A\u8FC7\u6279\u5904\u7406\u547D\u4EE4\u548C\u57FA\u672C\u7684\u7F16\
  \u8F91\u5668\u5B9E\u73B0\u3002\u73B0\u4ECA\uFF0CJavaScript \u4E2D\u63D0\u4F9B\u4E86\
  \ `.replace()` \u65B9\u6CD5\u7528\u4E8E\u6587\u672C\u66FF\u6362\u3002\u8FD9\u4E2A\
  \u65B9\u6CD5\u53EF\u63A5\u53D7\u5B57\u7B26\u4E32\u6216\u6B63\u5219\u8868\u8FBE\u5F0F\
  \u4F5C\u4E3A\u641C\u7D22\u53C2\u6570\uFF0C\u5B9E\u73B0\u7075\u6D3B\u7684\u6587\u672C\
  \u64CD\u4F5C\u3002\u9664\u4E86 `.replace()`, \u66FF\u4EE3\u65B9\u6CD5\u5982\u4F7F\
  \u7528 `.split()` \u548C `.join()` \u7EC4\u5408\u66FF\u6362\u4E5F\u5B58\u5728\uFF0C\
  \u4F46 `.replace()` \u66F4\u76F4\u89C2\u3001\u65B9\u4FBF\u3002"
title: "\u641C\u7D22\u548C\u66FF\u6362\u6587\u672C"
weight: 10
---

## How to: (如何操作：)
```javascript
// 搜索文本并替换
let text = "Hello World! Programming is fun.";
let newText = text.replace("World", "Mandarin Reader");

console.log(newText); // 输出: Hello Mandarin Reader! Programming is fun.
```

```javascript
// 使用正则表达式全局替换文本
let text = "Apples are round, and apples are juicy.";
let newText = text.replace(/apples/gi, "oranges");

console.log(newText); // 输出: Oranges are round, and oranges are juicy.
```

## Deep Dive (深入了解)
搜索和替换文本的需求诞生于早期编程的日子，当时通过批处理命令和基本的编辑器实现。现今，JavaScript 中提供了 `.replace()` 方法用于文本替换。这个方法可接受字符串或正则表达式作为搜索参数，实现灵活的文本操作。除了 `.replace()`, 替代方法如使用 `.split()` 和 `.join()` 组合替换也存在，但 `.replace()` 更直观、方便。

在处理复杂模式时，正则表达式（RegEx）变得无比强大。使用正则表达式的 `g` 标志进行全局搜索和替换，以及 `i` 标志忽略大小写。值得注意的是，`.replace()` 方法默认只替换第一个匹配项，除非使用正则表达式的全局 (`g`) 标志。

## See Also (另请参阅)
- [MDN Web Docs: String.prototype.replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [Regular Expressions (RegEx) Guide](https://www.regular-expressions.info/)
- [JavaScript String Methods](https://www.w3schools.com/js/js_string_methods.asp)
