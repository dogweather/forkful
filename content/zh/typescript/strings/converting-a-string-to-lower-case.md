---
date: 2024-01-20 17:39:29.311407-07:00
description: "\u8F6C\u6362\u5B57\u7B26\u4E32\u4E3A\u5C0F\u5199\u610F\u5473\u7740\u5C06\
  \u6240\u6709\u7684\u5927\u5199\u5B57\u6BCD\u6539\u4E3A\u5C0F\u5199\u3002\u7A0B\u5E8F\
  \u5458\u8FD9\u4E48\u505A\u53EF\u4EE5\u5B9E\u73B0\u5927\u5C0F\u5199\u4E0D\u654F\u611F\
  \u7684\u6BD4\u8F83\uFF0C\u63D0\u9AD8\u6570\u636E\u4E00\u81F4\u6027\u548C\u7528\u6237\
  \u4F53\u9A8C\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.457359-06:00'
model: gpt-4-1106-preview
summary: "\u8F6C\u6362\u5B57\u7B26\u4E32\u4E3A\u5C0F\u5199\u610F\u5473\u7740\u5C06\
  \u6240\u6709\u7684\u5927\u5199\u5B57\u6BCD\u6539\u4E3A\u5C0F\u5199\u3002\u7A0B\u5E8F\
  \u5458\u8FD9\u4E48\u505A\u53EF\u4EE5\u5B9E\u73B0\u5927\u5C0F\u5199\u4E0D\u654F\u611F\
  \u7684\u6BD4\u8F83\uFF0C\u63D0\u9AD8\u6570\u636E\u4E00\u81F4\u6027\u548C\u7528\u6237\
  \u4F53\u9A8C\u3002."
title: "\u5C06\u5B57\u7B26\u4E32\u8F6C\u6362\u4E3A\u5C0F\u5199"
weight: 4
---

## How to (怎么做)
在TypeScript中，使用`toLowerCase()`方法轻松转换字符串到小写：

```typescript
let greeting: string = "Hello, World!";
let lowerCaseGreeting: string = greeting.toLowerCase();
console.log(lowerCaseGreeting); // 输出: hello, world!
```

## Deep Dive (深入了解)
字符串转换为小写是编程历史中非常常见的操作。早期计算机只支持大写字母，进步之后才加入了大小写。这个功能在不同的编程语言中差不多是一样的，在TypeScript/JavaScript中，`toLowerCase()`方法是原型链上的方法，对于Unicode字符也同样有效。

### 替代方法
除了`toLowerCase()`，在一些情况下，你可能会用到正则表达式来替换特定模式的字符：

```typescript
let response: string = "OK!";
let lowerCaseResponse: string = response.replace(/[A-Z]/g, char => char.toLowerCase());
console.log(lowerCaseResponse); // 输出: ok!
```

### 实现细节
`toLowerCase()`会考虑特定区域性的字母，确保转换准确无误。例如，在土耳其语中，大写的'I'转换为小写不是'ı'，而是'i'。

## See Also (另请参阅)
- MDN文档关于`String.prototype.toLowerCase()`: [MDN toLowerCase](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- TypeScript官方文档：[TypeScript Handbook](https://www.typescriptlang.org/docs/)
- Unicode字符表：[Unicode Character Table](https://unicode-table.com/en/)
