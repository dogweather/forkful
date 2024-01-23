---
title:                "编写文本文件"
html_title:           "Arduino: 编写文本文件"
simple_title:         "编写文本文件"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
JavaScript 写文本文件就是在磁盘上创建或编辑纯文本内容。程序员这么做通常是为了记录信息、导出数据、或者是创建配置文件。

## How to: (如何做：)
以下示例使用 Node.js 的 `fs` 模块来写文件。

```Javascript
const fs = require('fs');

fs.writeFile('example.txt', '这是一个文本文件示例', function(err) {
    if (err) throw err;
    console.log('文件已被保存');
});
```

运行后会看到输出：
```
文件已被保存
```

## Deep Dive (深入了解)
- 历史上，JavaScript 是为了在浏览器中运行而设计，直接写文件是不被允许的。Node.js 出现让这成为可能。
- 除了 `fs.writeFile`，还可以使用 `fs.writeFileSync` 同步写文件，或是 `fs.createWriteStream` 创建可写流。
- 细节上，编码格式（默认 `utf-8`）、错误处理和文件路径都是写文件时要考虑的因素。

## See Also (另请参阅)
- Node.js 官方文档关于 `fs` 模块：[Node.js fs module](https://nodejs.org/api/fs.html)
- JavaScript 文件和流处理教程：[MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/API/File_and_Directory_Entries_API)
