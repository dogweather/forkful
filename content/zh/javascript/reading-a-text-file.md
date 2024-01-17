---
title:                "读取文本文件"
html_title:           "Javascript: 读取文本文件"
simple_title:         "读取文本文件"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

# read()函数：以简洁准确的风格用于读取文本文件

## What & Why?
read()函数是一个用于读取文本文件的方法，它可以从文本文件中读取数据并将其保存为变量，以供程序其他部分使用。程序员通常会使用这个函数来处理文件输入，例如读取用户输入、读取配置文件或者处理服务器响应。

## How to:
```Javascript
const fs = require('fs'); //引入文件系统模块
const fileName = 'example.txt'; //指定文件名或路径

//异步方式读取文件内容
fs. read(fileName, 'utf-8', (err, data) => {
  if (err) throw err; //如果有错误，则抛出错误
  console.log(data); //打印文件内容
});

//同步方式读取文件内容
const data = fs.readFileSync(fileName, 'utf-8'); //将文件内容保存到变量
console.log(data); //打印文件内容
```

使用```require()```方法引入文件系统模块，然后使用```read()```函数来读取文件内容。第一个参数是文件名或路径，第二个参数是编码格式（默认为utf-8），第三个参数是一个回调函数，当读取成功时，会将文件内容作为回调函数的第二个参数传递。使用```readFileSync()```函数可以同步地读取文件内容，将文件内容保存到变量后再打印出来。

## Deep Dive:
读取文本文件是编程中非常常见的操作，它能够帮助我们处理各种文件数据，从而简化程序的逻辑。在过去，程序员会使用类似```fopen()```和```fread()```等函数来读取文件内容，而现在有了更方便易用的```read()```函数来实现相同的功能。除此之外，也可通过使用不同的编码格式来读取不同格式的文件内容，例如使用```'binary'```编码来读取二进制文件，使用```'base64'```编码来读取Base64格式的文件。

## See Also:
- [fs模块文档](https://nodejs.org/api/fs.html)
- [Node.js文档](https://nodejs.org/en/docs/)