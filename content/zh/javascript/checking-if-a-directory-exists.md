---
title:                "检查目录是否存在"
date:                  2024-01-20T14:56:59.998406-07:00
html_title:           "Elixir: 检查目录是否存在"
simple_title:         "检查目录是否存在"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
检查目录是否存在是确认文件系统中某个路径的文件夹是否已经创建的过程。程序员这么做来避免文件操作错误，保证数据存储到正确的位置。

## 如何操作：
在JavaScript中检查目录是否存在，我们可以使用Node.js的`fs`模块。以下是步骤和例子：

```Javascript
const fs = require('fs');
const path = './path/to/directory';

try {
  if (fs.existsSync(path)) {
    console.log('目录存在！');
  } else {
    console.log('目录不存在。');
  }
} catch (err) {
  console.error('检查目录时出错:', err);
}
```

执行结果将会是：

如果目录存在：
```
目录存在！
```

如果目录不存在：
```
目录不存在。
```

## 深入探索：
在Node.js之前，检查目录是否存在不是那么直接。早期的方法需要尝试访问目录，然后处理失败的错误消息来判断。`fs.exists`已经在Node.js中被弃用，因为它不支持Promise并且无法区分不同的错误。

`fs.existsSync`的同步特性意味着它会阻塞其他代码执行，直到完成检查。在性能要求高的场合，这可能成为问题。

实际使用时，程序员可能会选用异步版本`fs.promises` API，或使用`fs.stat`和`fs.statSync`来获取目录信息，再判断其是否存在。

## 参考链接：
- Node.js官方fs模块文档：[https://nodejs.org/api/fs.html](https://nodejs.org/api/fs.html)
- 关于fs.existsSync的讨论：[https://nodejs.org/api/fs.html#fsexistssyncpath](https://nodejs.org/api/fs.html#fsexistssyncpath)
- 异步文件系统操作（`fs.promises` API）：[https://nodejs.org/api/fs.html#fspromises_stat_path_options](https://nodejs.org/api/fs.html#fspromises_stat_path_options)