---
title:                "检查目录是否存在"
html_title:           "TypeScript: 检查目录是否存在"
simple_title:         "检查目录是否存在"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 为什么要检查一个目录是否存在

当我们需要在我们的 TypeScript 代码中操作文件时，有时候需要先检查一个目录是否存在。这是为了避免在文件操作过程中出现错误，保证我们的代码能够顺利运行。

## 如何检查一个目录是否存在

在 TypeScript 中，我们可以使用内置的 `fs` 模块来检查一个目录是否存在。我们可以通过 `fs.stat()` 方法传入目录的路径来检查该目录是否存在，如下所示：

```TypeScript
import * as fs from 'fs';

const directoryPath = './myDirectory';

fs.stat(directoryPath, (err, stats) => {
  if (err) {
    // 如果出现错误，说明该目录不存在
    console.log('该目录不存在！');
  } else {
    // 否则，说明该目录存在
    console.log('该目录存在！');
  }
});
```

如果在目录不存在的情况下运行这段代码，我们会在控制台中看到 `该目录不存在！` 的输出，否则会看到 `该目录存在！` 的输出。

## 深入了解检查一个目录是否存在

在深入了解之前，首先让我们了解一下 `fs.stat()` 方法。它用于检索关于文件或目录的详细信息，并将其作为回调函数的第二个参数 `stats` 返回。如果在检索过程中出现错误，则会将 `err` 参数传入回调函数。

通过传入目录路径，我们可以使用 `stats.isDirectory()` 方法来判断是否存在一个目录。该方法会返回一个布尔值，如果是目录则为 `true`，否则为 `false`。

除了 `fs.stat()` 方法外，我们还可以使用 `fs.existsSync()` 方法来同步检查一个目录是否存在。该方法会返回一个布尔值，如果目录存在则为 `true`，否则为 `false`。示例如下：

```TypeScript
import * as fs from 'fs';

const directoryPath = './myDirectory';

if (fs.existsSync(directoryPath)) {
  console.log('该目录存在！');
} else {
  console.log('该目录不存在！');
}
```

## 参考资料

- [Node.js 文档 - fs 模块](https://nodejs.org/dist/latest-v14.x/docs/api/fs.html)
- [深入浅出 TypeScript（二） - Node.js 中的开发](https://zhuanlan.zhihu.com/p/64862638)

## 参见

- [使用 TypeScript 进行 Node.js 开发](https://github.com/Microsoft/TypeScript-Node-Starter)
- [TypeScript 中文网](https://www.tslang.cn/)