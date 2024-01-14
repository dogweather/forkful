---
title:    "Javascript: 检查目录是否存在"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# 为什么要检查目录是否存在？

在编写Javascript程序时，有时候我们需要检查某个特定的目录是否存在，这是很重要的一步。如果我们的程序需要读取或写入某个目录中的文件，那么在进行这些操作前，我们需要确认该目录确实存在。否则，如果目录不存在，程序就会出现错误。因此，检查目录是否存在可以帮助我们避免潜在的错误。

# 如何进行检查

要检查目录是否存在，我们可以使用Javascript中的内置模块fs（文件系统）。首先，我们需要引入fs模块：

```Javascript
const fs = require('fs');
```

然后，我们可以使用fs模块中的`existsSync()`方法来检查目录是否存在。该方法接收一个目录的路径作为参数，并返回一个布尔值，表明该目录是否存在。

```Javascript
const directoryPath = './myDirectory'; // 替换为你要检查的目录路径
const directoryExists = fs.existsSync(directoryPath);
console.log(directoryExists); // 如果myDirectory目录存在，将打印true，否则打印false
```

# 深入了解

除了使用fs模块之外，我们也可以使用第三方模块如`fs-extra`来检查目录是否存在。该模块提供了更多的功能，如递归地检查子目录的存在等。

此外，我们也可以通过使用try-catch语句来捕获目录不存在的错误。如果我们尝试读取或写入一个不存在的目录，程序会抛出错误，我们可以使用try-catch语句来处理该错误。

# 参考链接

- [Node.js文件系统模块文档](https://nodejs.org/dist/latest-v14.x/docs/api/fs.html)
- [fs-extra模块文档](https://github.com/jprichardson/node-fs-extra)
- [try-catch语句文档](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/try...catch)
- [Node.js中文文档](https://nodejs.org/zh-cn/docs/)