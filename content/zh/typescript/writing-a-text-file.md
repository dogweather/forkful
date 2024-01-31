---
title:                "编写文本文件"
date:                  2024-01-19
html_title:           "Arduino: 编写文本文件"
simple_title:         "编写文本文件"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
写文本文件是创建和保存字符数据到可读文件的过程。程序员这么做为了保存配置、导出报告或持久化数据。

## How to:
```TypeScript
import * as fs from 'fs';

let data = '学习TypeScript真有趣！';

// 异步写文件
fs.writeFile('example.txt', data, (err) => {
  if (err) throw err;
  console.log('文件已保存！');
});

// 同步写文件
try {
  fs.writeFileSync('exampleSync.txt', data);
  console.log('同步文件已保存！');
} catch (err) {
  console.error(err);
}
```

Sample Output:
```
文件已保存！
同步文件已保存！
```

## Deep Dive
TypeScript是JavaScript的超集，加入了类型系统和对ES6+的新特性支持。Node.js环境中，`fs`模块常用于文件操作，包括读写。使用TypeScript写文本文件时，通常用到`fs.writeFile`（异步）或`fs.writeFileSync`（同步）。可选用库如`fs-extra`或框架特有的方法，如`fs`模块的`fs.promises` API和ESM语法支持。

## See Also
- Node.js `fs`模块文档：[Node.js File System](https://nodejs.org/api/fs.html)
- TypeScript 官方手册：[TypeScript Documentation](https://www.typescriptlang.org/docs/)
- `fs-extra`库：[fs-extra GitHub](https://github.com/jprichardson/node-fs-extra)
