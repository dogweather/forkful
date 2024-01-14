---
title:                "Javascript: 读取文本文件"
simple_title:         "读取文本文件"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 为什么

读取文本文件是编程中一项重要的技能。它允许我们从本地驱动器或网络中检索文本数据，并以可读的格式进行处理。这个过程在数据处理和web开发中都非常常见。

## 如何操作

要读取文本文件，我们需要使用文件处理库中的`fs`模块。我们首先需要使用`require()`函数将该模块导入到我们的代码中。然后，我们可以使用`fs.readFile()`函数来读取文件。下面是一个示例代码：

```Javascript
const fs = require('fs');

fs.readFile('sample.txt', (err, data) => {
  if (err) throw err;
  console.log(data.toString());
});
```

在这个例子中，我们使用`readFile()`函数来读取名为`sample.txt`的文本文件。回调函数中的`data`参数包含了从文件中读取的数据。最后，我们使用`toString()`函数来将数据转换成字符串，并使用`console.log()`函数来打印出来。

让我们来看一下`sample.txt`文件的内容，并对比一下打印出的结果：

```
这是一个示例文本文件。
```

输出：

```
这是一个示例文本文件。
```

正如你所见，我们成功地读取了文本文件的内容。

## 深入了解

当使用`fs.readFile()`函数来读取文本文件时，我们需要注意以下几点：

- 第一个参数是要读取的文件的名称，可以是相对路径或绝对路径。
- 第二个参数是一个回调函数，它有两个参数：`err`和`data`。
- 如果文件读取失败，`err`参数将会被赋予一个错误对象。
- 如果文件读取成功，`data`参数将会包含从文件中读取的数据。
- 默认情况下，文件以字节流的形式被读取。因此，我们需要使用`toString()`函数来将其转换成字符串。

## 参考链接

- [Node.js文档 - fs模块](https://nodejs.org/dist/latest-v10.x/docs/api/fs.html)
- [阮一峰的《Node.js教程》](http://www.ruanyifeng.com/blog/2015/05/nodejs-fs.html)
- [MDN Web文档 - Node.js的fs（文件系统）模块](https://developer.mozilla.org/zh-CN/docs/Web/API/Node_FS)

## 查看还有

- [使用Node.js读取和写入文本文件](https://www.digitalocean.com/community/tutorials/how-to-read-and-write-files-in-node-js)
- [更多关于Node.js的教程和资源](https://nodejs.dev/)