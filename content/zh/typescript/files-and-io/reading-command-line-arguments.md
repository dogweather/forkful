---
date: 2024-01-20 17:56:55.846822-07:00
description: "\u5728\u547D\u4EE4\u884C\u4E2D\u8BFB\u53D6\u53C2\u6570\u8BA9\u7A0B\u5E8F\
  \u63A5\u6536\u7528\u6237\u8F93\u5165\u3002\u8FD9\u610F\u5473\u7740\u53EF\u4EE5\u5728\
  \u7A0B\u5E8F\u8FD0\u884C\u65F6\u5B9A\u5236\u884C\u4E3A\uFF0C\u975E\u5E38\u7075\u6D3B\
  \u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.489206-06:00'
model: gpt-4-1106-preview
summary: "\u5728\u547D\u4EE4\u884C\u4E2D\u8BFB\u53D6\u53C2\u6570\u8BA9\u7A0B\u5E8F\
  \u63A5\u6536\u7528\u6237\u8F93\u5165\u3002\u8FD9\u610F\u5473\u7740\u53EF\u4EE5\u5728\
  \u7A0B\u5E8F\u8FD0\u884C\u65F6\u5B9A\u5236\u884C\u4E3A\uFF0C\u975E\u5E38\u7075\u6D3B\
  \u3002."
title: "\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570"
weight: 23
---

## What & Why? (是什么？为什么？)
在命令行中读取参数让程序接收用户输入。这意味着可以在程序运行时定制行为，非常灵活。

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
