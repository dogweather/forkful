---
title:                "检查目录是否存在"
html_title:           "PHP: 检查目录是否存在"
simple_title:         "检查目录是否存在"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？

检查目录是否存在是一个常见的编程任务，通过在运行时查询文件系统，程序可以确认特定目录是否存在。在对文件系统进行操作之前进行检查可以帮助我们避免出现错误，例如尝试在不存在的目录下创建文件。

## 如何实现

在TypeScript中，我们可以使用`fs`模块的`existsSync`函数来检查目录是否存在。这是同步版本的函数，但也有一个异步版本`exists`。

```TypeScript
import { existsSync } from 'fs';

if (existsSync('/path/to/directory')) {
  console.log('Directory exists');
} else {
  console.log('Directory not found');
}
```

运行以上代码，你将在控制台中看到`Directory exists`或`Directory not found`的输出。

## 深度解析

在Node.js的早期版本中，`fs.exists`和`fs.existsSync`是推荐的方式来检查文件和目录是否存在。但是，由于一些设计上的问题，官方文档已经不再推荐使用这些方法，并推荐使用`fs.access`和`fs.accessSync`。这两种函数更具可靠性，可以检查文件或目录是否存在，同时还可以确认程序是否有权访问它们。

```TypeScript
import { accessSync, constants } from 'fs';

try {
  accessSync('/path/to/directory', constants.F_OK);
  console.log('Directory exists and is accessible');
} catch (err) {
  console.log('Directory not found or inaccessible');
}
```

## 另请参见

为了提供更细致的检查，你可以使用`fs.stat` 或 `fs.statSync`函数来获取关于文件或目录的详细信息。

请参考以下链接来获得更多的信息：

- Node.js FileSystem API文档: [https://nodejs.org/api/fs.html](https://nodejs.org/api/fs.html)
- TypeScript官方网站: [https://www.typescriptlang.org/](https://www.typescriptlang.org/)