---
title:                "TypeScript: 检查目录是否存在。"
programming_language: "TypeScript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

为什么：检查目录是否存在是编程中非常常见的操作。当我们需要在代码中操作文件或目录时，首先需要确认目录是否存在。这可以帮助我们避免因为不存在的目录而导致的错误。

如何：下面我们将使用 TypeScript 来演示如何检查目录是否存在。

```TypeScript
import fs from 'fs';

const directoryPath = './test';

// 使用 fs.exists() 方法来检查目录是否存在
fs.exists(directoryPath, (exists) => {
  if (exists) {
    console.log(`The directory ${directoryPath} exists.`);
  } else {
    console.log(`The directory ${directoryPath} does not exist.`);
  }
});

// 使用 fs.accessSync() 方法来检查目录是否存在
try {
  fs.accessSync(directoryPath);
  console.log(`The directory ${directoryPath} exists.`);
} catch (err) {
  console.log(`The directory ${directoryPath} does not exist.`);
}
```

输出结果：

```TypeScript
The directory ./test exists.
The directory ./test does not exist.
```

深入了解：在 TypeScript 中，我们可以使用两种方法来检查目录是否存在：`fs.exists()` 和 `fs.accessSync()`。`fs.exists()` 是一个异步方法，它采用目录路径和回调函数作为参数。当目录存在时，回调函数的第一个参数为 `true`，否则为 `false`。另一种方法是 `fs.accessSync()`，它是一个同步的方法，它直接返回一个错误对象，通过捕获这个错误来判断目录是否存在。

查看相关信息：如果你想进一步了解有关检查目录是否存在的信息，可以参考下面的链接：

[Synchronously/Asynchronously check whether directory exists in TypeScript](https://coderrocketfuel.com/article/synchronously-asynchronously-check-whether-directory-exists-in-typescript)

[Node.js | fs.existsSync() Method](https://www.geeksforgeeks.org/node-js-fs-existssync-method/)

[Node.js | fs.accessSync() Method](https://www.geeksforgeeks.org/node-js-fs-accesssync-method/)

可见也就是这样。Happy coding!

另见：

[fs.exists vs fs.existsSync vs fs.statSync](https://stackoverflow.com/questions/17699599/fs-exists-vs-fs-existssync-vs-fs-statsync-in-node-js)

[Node.js | fs.existsSync() vs fs.accessSync()](https://www.geeksforgeeks.org/node-js-fs-existssync-vs-fs-accesssync/)