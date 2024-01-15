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

# 为什么

在编写Javascript程序时，我们经常需要检查某个目录是否存在。这可以帮助我们确定文件是否已经存在，以便我们可以做出相应的处理。所以，检查目录存在性在编写JavaScript程序时非常有用。

## 如何

假设我们有一个目录路径的变量，我们可以使用 node.js 内置的 `fs` 模块来检查它是否存在。下面是一个简单的例子：

```javascript
const fs = require('fs');

// 假设我们有一个目录路径的变量
const directoryPath = './myDirectory';

// 使用 `fs.existsSync()` 方法来检查目录是否存在
if (fs.existsSync(directoryPath)) {
  console.log('目录已存在');
} else {
  console.log('目录不存在');
}
```

当我们运行这段代码时，如果 `myDirectory` 目录已经存在，那么控制台将输出 `目录已存在`。如果该目录不存在，则会输出 `目录不存在`。

## 深入探讨

在上面的例子中，我们使用了 `fs.existsSync()` 方法来检查目录的存在性。该方法将会返回一个布尔值，如果目录存在则为 `true`，否则为 `false`。我们也可以使用 `fs.stat()` 方法来检查目录的存在性。下面是一个使用该方法的例子：

```javascript
const fs = require('fs');

// 假设我们有一个目录路径的变量
const directoryPath = './myDirectory';

// 使用 `fs.stat()` 方法来检查目录是否存在
fs.stat(directoryPath, (err, stats) => {
  if (err) {
    console.error(err);
    return;
  }

  // 如果目录存在，则 `stats` 对象中会有相关的信息
  console.log(stats.isDirectory()); // true
  console.log(stats.size); // 0 (目录大小为 0)
});
```

`fs.stat()` 方法将会返回一个 `stats` 对象，它包含了该目录的相关信息，比如大小、创建时间等。我们通过调用返回的 `isDirectory()` 方法，可以判断目录是否为一个目录。

# 参考链接

- [Node.js Docs: `fs` 模块](https://nodejs.org/api/fs.html)
- [Node.js Docs: `fs.existsSync()` 方法](https://nodejs.org/api/fs.html#fs_fs_existssync_pathoptions)
- [Node.js Docs: `fs.stat()` 方法](https://nodejs.org/api/fs.html#fs_fs_stat_path_options_callback)

# 查看更多

- [如何使用Node.js创建和管理文件](https://www.freecodecamp.org/news/nodejs-create-file-in-readable-writable-mode/)
- [使用Node.js实现文件系统操作](https://www.tutorialspoint.com/nodejs/nodejs_file_system.htm)