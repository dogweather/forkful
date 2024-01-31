---
title:                "打印调试输出"
date:                  2024-01-20T17:53:21.231953-07:00
model:                 gpt-4-1106-preview
simple_title:         "打印调试输出"

category:             "TypeScript"
tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
打印调试输出就是在代码中插入语句，让程序运行时显示信息。程序员这样做是为了理解代码如何运行，找出并修正错误。

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
