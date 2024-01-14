---
title:    "TypeScript: 读取文本文件"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 为什么

读取文本文件是软件开发中一个基本的任务，它可以帮助我们处理大量的文本数据，如日志文件、配置文件等。当我们需要访问这些文本数据时，就可以使用 TypeScript 来读取文本文件。通过阅读本文，您将学习到如何使用 TypeScript 读取文本文件，让您在软件开发中更加灵活和高效。

## 如何

首先，我们需要创建一个 TypeScript 文件，命名为 `textReader.ts`，用于存放我们的读取文本文件的代码。然后，在我们的 TypeScript 文件中，我们需要使用 Node.js 的内置模块 `fs` 来读取文本文件。代码如下所示：

```TypeScript
import * as fs from 'fs';

fs.readFile('input.txt', 'utf-8', (err, data) => {
  if (err) throw err;
  console.log(data);
});

```

上述代码中，我们使用 `readFile` 方法来读取名为 `input.txt` 的文本文件，第二个参数为 `utf-8` 表示编码格式为 UTF-8，并在回调函数中处理读取到的文本数据。当我们运行该 TypeScript 文件时，控制台会输出文本文件中的内容。

## 深入探讨

除了使用 `fs` 模块的 `readFile` 方法，我们还可以使用 `createReadStream` 方法来逐行读取文本文件。代码如下所示：

```TypeScript
import * as fs from 'fs';

const readStream = fs.createReadStream('input.txt');

readStream.on('data', (chunk) => {
  console.log(chunk);
});

readStream.on('end', () => {
  console.log('Finished reading file');
});

```

上述代码中，我们通过 `createReadStream` 方法创建了一个可读流，并使用 `on` 方法来监听每次读取到的数据（以 `chunk` 的形式返回），以及当文件读取完毕时的事件。这样做可以避免一次性读取整个文本文件，对于大型文本文件可以提高读取的效率。

## 参考链接

- [Node.js 文档（Fs 模块）](https://nodejs.org/api/fs.html)
- [深入理解 TypeScript](https://jkchao.github.io/typescript-book-chinese/)

## 参见

- [使用 TypeScript 写一个简单的 HTTP 服务器](https://www.example.com/ts-http-server)
- [学习 TypeScript 的官方网站](https://www.typescriptlang.org/)