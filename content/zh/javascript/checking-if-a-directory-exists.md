---
title:                "Javascript: 检查目录是否存在"
programming_language: "Javascript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 为什么：为什么要检查目录是否存在？

在编写Javascript程序时，有时我们需要检查某个特定的目录是否存在，这可以帮助我们确定程序是否可以顺利运行。如果目录不存在，程序可能会出现错误，因此检查目录是否存在可以帮助我们提前发现问题并解决它们。

## 如何做：

接下来，我将向大家介绍如何在Javascript中检查目录是否存在。我们可以使用Node.js中的`fs`模块来实现。首先，我们需要引入`fs`模块并使用`existsSync()`函数来检查目录是否存在，代码示例如下：

```Javascript
const fs = require('fs');
if (fs.existsSync('path/to/directory')) {
  console.log('目录存在'); 
} else {
  console.log('目录不存在'); 
}
```

如果目录存在，`existsSync()`函数将返回`true`，否则将返回`false`。通过这种方式，我们可以根据结果来进行下一步的操作，从而避免程序出现错误。

## 深入了解：

当我们调用`existsSync()`函数时，实际上是在检查给定路径是否存在指定的目录或者文件。如果`existsSync()`返回`false`，我们还可以使用`mkdirSync()`函数来创建新的目录，代码示例如下：

```Javascript
if (!fs.existsSync('path/to/directory')) {
  fs.mkdirSync('path/to/directory'); 
}
```

通过这种方式，我们可以动态地创建目录，以确保程序的顺利运行。

## 参考链接：

了解更多关于Node.js中`fs`模块的信息，请参考以下链接：

- [Node.js文档中关于`fs`模块的介绍](https://nodejs.org/dist/latest-v14.x/docs/api/fs.html)
- [如何在Node.js中检查文件或目录是否存在](https://www.geeksforgeeks.org/node-js-fs-exists-method/)
- [使用Node.js中的`fs`模块来创建新的目录](https://www.digitalocean.com/community/tutorials/nodejs-create-file-system)
## 参考链接：

了解更多关于Node.js中fs模块的信息，请参考以下链接：

- [Node.js文档中关于fs模块的介绍](https://nodejs.org/dist/latest-v14.x/docs/api/fs.html)
- [如何在Node.js中检查文件或目录是否存在](https://www.geeksforgeeks.org/node-js-fs-exists-method/)
- [使用Node.js中fs模块来创建新的目录](https://www.digitalocean.com/community/tutorials/nodejs-create-file-system)