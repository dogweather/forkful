---
title:                "写入标准错误"
html_title:           "Arduino: 写入标准错误"
simple_title:         "写入标准错误"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
写入标准错误（stderr）是输出错误信息或日志到一个专用的错误流。程序员这样做以区分正常输出和错误，有助于调试和日志记录。

## How to: (如何操作：)
```typescript
console.error('错误信息：操作失败');

process.stderr.write('警告：内存不足\n');

// 输出示例：
// 错误信息：操作失败
// 警告：内存不足
```

## Deep Dive (深入探究)
在UNIX哲学中，标准错误流（stderr）是个重要概念，允许程序员将错误信息独立于标准输出（stdout）。除了`console.error`和`process.stderr.write`，Node.js提供事件和流API进行更精细的控制。标准输出可用于命令行工具的管道操作，而标准错误则用来输出错误而不中断管道流。

## See Also (另请参阅)
- 深入了解Node.js控制台: [Node.js Console Class](https://nodejs.org/api/console.html#console_class_console)
