---
title:                "检查目录是否存在"
date:                  2024-01-20T14:58:54.953171-07:00
html_title:           "Elixir: 检查目录是否存在"
simple_title:         "检查目录是否存在"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (什么以及为什么？)
检查目录是否存在是指用代码确认文件系统中某个特定目录是否已经建立。程序员这么做以避免读写错误，确保数据被正确地存储和获取。

## How to: (如何操作：)
TypeScript中，你可以通过`fs`模块来检查目录是否存在：

```TypeScript
import fs from 'fs';

const directoryPath = './path/to/your/directory';

// 异步检查
fs.access(directoryPath, fs.constants.F_OK, (err) => {
  if (err) {
    console.error(`${directoryPath} does not exist`);
  } else {
    console.log(`${directoryPath} exists`);
  }
});

// 同步检查
if (fs.existsSync(directoryPath)) {
  console.log(`${directoryPath} exists`);
} else {
  console.error(`${directoryPath} does not exist`);
}
```
输出可能会是：
```
./path/to/your/directory exists
```
或者
```
./path/to/your/directory does not exist
```

## Deep Dive (深入了解)
在Node.js和TypeScript的早期版本中，大家习惯使用`fs.existsSync`来同步检查文件或目录是否存在。然而，这种方法会阻塞事件循环，影响性能。现代的实践倾向于使用异步的`fs.access`方法，这样即使在检查的时候，程序也能处理其他任务。

替代方案包括使用第三方库如`fs-extra`或使用新的`fs.promises` API以支持`async/await`。ES的新版本甚至允许你直接在`import`语句中用异步方式动态加载模块，这也可以用于检查。

实现上，`fs.access`使用底层系统调用来进行检查，它还可以检查读写权限等。

## See Also (另请参阅)
- [Node.js `fs` documentation](https://nodejs.org/api/fs.html)
- [`fs-extra` GitHub repository](https://github.com/jprichardson/node-fs-extra)
- [TypeScript Handbook](https://www.typescriptlang.org/docs/handbook/intro.html)
