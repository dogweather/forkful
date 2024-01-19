---
title:                "读取文本文件"
html_title:           "Kotlin: 读取文本文件"
simple_title:         "读取文本文件"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 什么和为什么？

读取文本文件即指让程序读取并理解文本文件中的内容。程序员之所以需要这样做，是因为通过读取文本文件，我们可以从中获取存储在其中的数据或信息。

## 如何操作：

以下是使用Node.js读取文本文件的代码示例。我们将使用Node.js内置的'fs'模块。

```Javascript
var fs = require('fs');

fs.readFile('test.txt', 'utf8', function(err, data) {
  if (err) throw err;
  console.log(data);
});
```
运行以上代码时，它将读取同一目录下名为'test.txt'的文本文件，并将内容输出到控制台。注意，这是异步执行的。因此，任何在回调函数外部依赖此数据的代码可能无法正确工作。

## 深入探讨：

读取文件是计算机程序的基础操作之一，它的历史可追溯到早期计算机系统。当时，文本文件是数据和指令的主要储存介质。

你可以选择使用不同的方法读取文件，例如，除了上面提到的异步读取，Node.js还提供了同步读取方法：

```Javascript
var fs = require('fs');

var data = fs.readFileSync('test.txt', 'utf8');
console.log(data);

```
此代码将同步读取文件，并将结果存储在变量“data”中。需要注意的是，同步读取将阻塞后续执行，直到文件被完全读取。这样做可能会阻碍程序的其他部分。

## 另请参阅：

要获得更多关于JavaScript和Node.js读取文件的信息，可以查看以下链接：

- Node.js官方文档(fs模块)：[https://nodejs.org/api/fs.html](https://nodejs.org/api/fs.html)
- MDN关于JavaScript的详细指南：[https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Guide](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Guide)