---
title:    "Javascript: 撰写文本文件"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

#为什么#

在编程中，有时候我们需要将数据存储在文本文件中。这样做的好处是可以方便地读取和编辑数据，并且可以避免在每次运行程序时重新输入数据。

##如何操作？

要创建一个文本文件，我们可以使用Javascript中的文件系统模块“fs”。首先，我们需要使用`require()`函数导入文件系统模块。然后，我们使用`fs.writeFile()`函数来创建一个新的文本文件，并指定文件的名称和内容。具体代码如下：

```Javascript
const fs = require('fs');

fs.writeFile('example.txt', '这是一个示例文本文件。', (err) => {
  if (err) {
    console.log(err);
  } else {
    console.log('文本文件已成功创建。');
  }
});
```

运行这段代码后，会在当前目录下创建一个名为“example.txt”的文本文件，并将内容保存在其中。如果该目录下已存在同名文件，那么该文件的内容将被覆盖。

要向文本文件中追加内容，我们可以使用`fs.appendFile()`函数。这个函数的用法和`fs.writeFile()`类似，只是它会在已有文件的末尾添加新内容，而不是覆盖原有的内容。具体代码如下：

```Javascript
fs.appendFile('example.txt', '这是新添加的内容。', (err) => {
  if (err) {
    console.log(err);
  } else {
    console.log('内容已成功添加到文本文件中。');
  }
});
```

除了创建和追加内容外，我们还可以使用`fs.readFile()`函数来读取文本文件中的内容。具体代码如下：

```Javascript
fs.readFile('example.txt', 'utf8', (err, data) => {
  if (err) {
    console.log(err);
  } else {
    console.log(data);
  }
});
```

这里的`utf8`是指定读取文件时的编码格式，使得我们能够正确地读取中文内容。

##深入了解

在创建文本文件时，我们还可以指定文件的编码格式、文件的权限等选项。例如，我们可以使用`fs.writeFile()`函数的第三个参数来指定文件的编码格式和权限。具体代码如下：

```Javascript
fs.writeFile('example.txt', '这是一个示例文本文件。', {encoding: 'utf8', mode: 0o666}, (err) => {
  if (err) {
    console.log(err);
  } else {
    console.log('文本文件已成功创建。');
  }
});
```

此外，我们还可以使用`fs.writeFileSync()`函数来创建或写入文件，它和`fs.writeFile()`的用法类似，只是没有回调函数，会直接返回一个错误信息。

#如此而已！#

虽然在这篇文章中我们只介绍了一些基础的操作，但是文本文件的创建、读取和写入是非常常用的操作。希望这篇文章能够帮助大家在编程时更加方便地操作文本文件。

#请参考#

- [Node.js文档: fs模块](https://nodejs.org/dist/latest/docs/api/fs.html)
- [菜鸟教程: Node.js 文件系统](https://www.runoob.com/nodejs/nodejs-fs.html)
- [掘金: NodeJS文件操作](https://juejin.im/post/5a8934a16fb9a0634b71e657)