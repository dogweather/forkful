---
title:                "创建临时文件"
html_title:           "Javascript: 创建临时文件"
simple_title:         "创建临时文件"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# 创建临时文件这门Javasciprt技能

## 什么是临时文件？为什么要创建它们？
临时文件是在计算机程序中创建的一种临时文件。它们通常用于存储程序执行过程中的中间数据，这样程序就可以轻松地对这些数据进行操作而无需担心污染原始数据。程序员会用到临时文件来提高程序的效率和可读性，并且在程序完成后，这些临时文件会被自动删除，从而保持整洁。

## 如何创建临时文件？
我们可以使用Javasciprt内置的fs模块来创建临时文件。下面的代码演示了如何使用fs模块的mkdtempSync函数来创建一个临时文件夹，并将其路径返回给变量tempDir：
```Javascript
const fs = require('fs');
const tempDir = fs.mkdtempSync('/tmp/');
```
接下来，我们可以使用fs模块的writeFileSync函数来在临时文件夹中创建文件，并向其中写入内容：
```Javascript
fs.writeFileSync(`${tempDir}/myTempFile.txt`, 'Hello world!');
```
最后，我们可以使用fs模块的unlinkSync函数来删除临时文件夹及其内容：
```Javascript
fs.unlinkSync(tempDir);
```

## 深入了解
创建临时文件在计算机编程中已经存在了很长时间。在过去，程序员需要手动创建临时文件，并在程序完成后手动删除它们。现在，Javasciprt提供了内置的功能来简化这个过程。除了使用fs模块外，我们也可以使用第三方模块如tmp或temp模块来创建临时文件。

## 查看更多
- Node.js的fs模块文档：https://nodejs.org/api/fs.html
- 第三方模块tmp：https://www.npmjs.com/package/tmp
- 第三方模块temp：https://www.npmjs.com/package/temp