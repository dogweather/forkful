---
date: 2024-01-20 17:56:55.846822-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C\uFF1A) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.489206-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570"
weight: 23
---

## How to: (如何操作：)
```TypeScript
// app.ts
const args = process.argv.slice(2);

console.log('Command Line Arguments:', args);
```

运行 `tsc app.ts` 编译，然后执行 `node app.js hello world`：

```
Command Line Arguments: [ 'hello', 'world' ]
```

## Deep Dive (深入了解)
命令行参数从早期 Unix 传承而来，非常通用。Node.js 在 `process.argv` 中提供了这些参数。`slice(2)` 是因为前两个参数是 Node 本身和脚本文件路径。可选的库如 `minimist` 提供更多解析选项。TypeScript 作为 JavaScript 的超集，使用方式相似但提供了类型安全。

## See Also (另请参阅)
- [Node.js documentation on `process.argv`](https://nodejs.org/docs/latest/api/process.html#process_process_argv)
- [Command Line Arguments in TypeScript on Stack Overflow](https://stackoverflow.com/questions/tagged/typescript+command-line-arguments)
