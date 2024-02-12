---
title:                "读取命令行参数"
aliases:
- /zh/typescript/reading-command-line-arguments/
date:                  2024-01-20T17:56:55.846822-07:00
model:                 gpt-4-1106-preview
simple_title:         "读取命令行参数"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

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
