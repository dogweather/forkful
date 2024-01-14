---
title:                "TypeScript: 标准出错编程指南"
programming_language: "TypeScript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 为什么要写入标准错误

你可能经常在阅读代码时遇到过`console.log()`这样的语句，在调试时它对于打印出变量的值非常方便。但有时候，我们可能想要输出一些警告信息或者错误信息，这时候就可以使用标准错误相关的方法。写入标准错误可以帮助我们更好地调试代码，定位问题，也可以作为向用户展示错误信息的一种方式。

## 如何写入标准错误

首先，让我们来看一下基本的写入标准错误的方法： `console.error()`。它接受一个参数，即要输出的信息，可以是字符串、数字、布尔值等。我们可以使用 TypeScript 来演示一下：

```TypeScript
console.error("这是一个错误信息");
// Output: 这是一个错误信息
console.error(404);
// Output: 404
console.error(true);
// Output: true
```

除了基本的输出方法，我们还可以使用 `console.warn()` 来输出警告信息，它和 `console.error()` 的用法类似。另外，我们也可以通过 `console.assert()` 来断言一些条件，并输出错误信息，如果条件不满足，则会输出信息。

```TypeScript
console.assert(1>2, "这里发生了一个错误");
// Output: 这里发生了一个错误

console.assert(1===1, "这里发生了一个错误");
// No output
```

## 深入了解

在 TypeScript 中，我们也可以使用 `process.stderr` 来获取标准错误输出流，然后通过 `write()` 方法来写入信息。这种方式更加灵活，也更适合一些复杂的场景。

```TypeScript
import * as fs from "fs";

const errorStream = fs.createWriteStream("error.log");

process.stderr.write("这条信息会被写入到 error.log 文件中", errorStream);
```

除了这些基本用法以外，我们还可以通过配置 `tsconfig.json` 文件中的 `--emitOnError` 选项来决定编译时是否将错误信息输出到标准错误。

```
// tsconfig.json

{
  "compilerOptions": {
    "emitOnError": true
  }
}
```

## 参考链接

- [TypeScript 文档: Console](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-4-1.html#console)
- [MDN 文档: console.error()](https://developer.mozilla.org/en-US/docs/Web/API/Console/error)
- [Node.js 文档: process.stderr](https://nodejs.org/dist/latest-v12.x/docs/api/process.html#process_process_stderr)

## 参见

- [Markdown 语法指南](https://markdown.cn/?spm=5176.100239.blogcont49270.10.nmQFJn)