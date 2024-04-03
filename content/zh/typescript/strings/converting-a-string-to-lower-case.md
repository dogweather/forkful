---
date: 2024-01-20 17:39:29.311407-07:00
description: "How to (\u600E\u4E48\u505A) \u5728TypeScript\u4E2D\uFF0C\u4F7F\u7528\
  `toLowerCase()`\u65B9\u6CD5\u8F7B\u677E\u8F6C\u6362\u5B57\u7B26\u4E32\u5230\u5C0F\
  \u5199\uFF1A."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.457359-06:00'
model: gpt-4-1106-preview
summary: "\u5728TypeScript\u4E2D\uFF0C\u4F7F\u7528`toLowerCase()`\u65B9\u6CD5\u8F7B\
  \u677E\u8F6C\u6362\u5B57\u7B26\u4E32\u5230\u5C0F\u5199\uFF1A."
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
