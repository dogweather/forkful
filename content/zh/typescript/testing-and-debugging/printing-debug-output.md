---
date: 2024-01-20 17:53:21.231953-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C\uFF1A) \u5728TypeScript\u91CC, \u4F7F\
  \u7528`console.log`\u6765\u6253\u5370\u8C03\u8BD5\u4FE1\u606F\u3002\u66F4\u9AD8\u7EA7\
  \u7684\u8C03\u8BD5\uFF0C\u7528`console.debug`, `console.info`, `console.warn`, `console.error`."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:47.798427-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u64CD\u4F5C\uFF1A) \u5728TypeScript\u91CC, \u4F7F\u7528`console.log`\u6765\
  \u6253\u5370\u8C03\u8BD5\u4FE1\u606F\u3002\u66F4\u9AD8\u7EA7\u7684\u8C03\u8BD5\uFF0C\
  \u7528`console.debug`, `console.info`, `console.warn`, `console.error`."
title: "\u6253\u5370\u8C03\u8BD5\u8F93\u51FA"
weight: 33
---

## How to: (如何操作：)
在TypeScript里, 使用`console.log`来打印调试信息。更高级的调试，用`console.debug`, `console.info`, `console.warn`, `console.error`.

```typescript
function addNumbers(a: number, b: number): number {
    console.log(`Adding ${a} + ${b}`);
    return a + b;
}

const sum = addNumbers(5, 3);
console.log(`Sum: ${sum}`);
```

输出：
```
Adding 5 + 3
Sum: 8
```

## Deep Dive (深入探究)
打印调试输出源自早期编程，那时候调试工具不多。现在虽有IDE和调试器，`console.log`还是简便的调试方式。可是，别过度依赖它，这可能导致代码混乱，而且影响性能。考虑用更现代的如`debug`包或者使用IDE的调试功能。

## See Also (另请参阅)
- Mozilla Developer Network (MDN) on `console`: [MDN console](https://developer.mozilla.org/en-US/docs/Web/API/Console)
- TypeScript Handbook: [TypeScript Handbook](https://www.typescriptlang.org/docs/handbook/intro.html)
- Node.js `debug` Module: [Node.js debugging](https://nodejs.org/api/debugger.html)
