---
title:                "检查目录是否存在"
html_title:           "Javascript: 检查目录是否存在"
simple_title:         "检查目录是否存在"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？

校验目录存在是用来确认指定的目录是否存在于文件系统中；程序员通常这样做是为了避免因为认为目录存在而操作时引起的错误。

## 如何操作：

以下是使用Javascript来判断目录是否存在的一个简单例子：

```Javascript
var fs = require('fs');

fs.stat('/path/to/directory', function(err, stats) {
    if (err) {
        if(err.code === 'ENOENT') {
            console.log("Directory does not exist.");
        } else {
            console.error("An error occurred: ", err);
        }
    } else if (stats.isDirectory()) {
        console.log("Directory exists.");
    }
});
```

如果目录不存在，将输出“Directory does not exist.”，如果存在，则会输出“Directory exists.”。

## 深度揭秘：

历史背景：在早期的node.js版本中，你可以使用`fs.exists()`来检查目录是否存在，但是该方法已经被标记为废弃，因为其设计并不符合典型的Node.js回调模式。

替代方案： 使用`fs.existsSync()`来同步地检查一个目录是否存在。但要注意，这个方法会阻塞事件循环，直到操作完成。

```Javascript
var fs = require('fs');

if (fs.existsSync('/path/to/directory')) {
    console.log("Directory exists.");
} else {
    console.log("Directory does not exist.");
}
```
实现细节： `fs.stat()`或者`fs.existsSync()`方法的底层都是有关file system的系统调用。这些方法将返回文件系统对象，你可以利用这些对象的方法(如`.isDirectory()`)来判断是否是目录。

## 参看资料：

1. [Node.js fs library](https://nodejs.org/api/fs.html)
2. [File system in Node.js](https://nodejs.dev/learn/the-nodejs-fs-module)

-