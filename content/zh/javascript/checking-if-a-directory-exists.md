---
title:                "Javascript: 检查文件夹是否存在"
simple_title:         "检查文件夹是否存在"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# 为什么要检查文件夹是否存在？

当编写Javascript代码时，有时候需要检测某个文件夹是否存在。这样可以避免在程序运行过程中出现错误。如果文件夹不存在，我们可以提前处理，而不是运行到错误的代码而导致程序崩溃。

## 如何进行检查？

首先，我们需要使用内置的fs模块来操作文件和文件夹。然后，我们可以使用 `fs.exists()` 方法来检测文件夹是否存在。下面是一个例子：

```Javascript
// 引入fs模块
const fs = require('fs');

// 检查文件夹是否存在
fs.exists('my_directory', (exists) => {
  if (exists) {
    console.log('文件夹已存在');
  } else {
    console.log('文件夹不存在');
  }
});
```

运行上面的代码，如果 `my_directory` 文件夹存在，那么输出会是 `文件夹已存在`，否则会是 `文件夹不存在`。另外，我们也可以使用 `fs.existsSync()` 同步方法来进行检查。语法如下：

```Javascript
fs.existsSync(path)
```

其中，`path` 是要检查的文件夹路径。如果文件夹存在，该方法会返回 `true`，否则返回 `false`。

## 深入了解

在 Javascript 中，检查文件夹是否存在的方法主要有两种，即异步方法 `fs.exists()` 和同步方法 `fs.existsSync()`。这两种方法都接受一个回调函数作为参数，以在检查完成后进行响应。值得注意的是，在使用同步方法时，程序会堵塞在该行代码，直到检查完成才会继续执行。因此，如果不想影响程序的运行效率，建议使用异步方法。

此外，有时候我们还需要检查文件夹的权限，以便在程序中做相应的处理。可以使用 `fs.access()` 方法来检查文件夹是否有指定权限。语法如下：

```Javascript
fs.access(path, mode, (err) => {
  if (err) {
    console.log('文件夹没有指定权限');
  } else {
    console.log('文件夹有指定权限');
  }
});
```

在以上代码中，`mode` 参数可以指定所需的权限，如 `fs.W_OK` 表示可写权限。更多的权限参考可以查看 [Node.js官方文档](https://nodejs.org/api/fs.html#fs_file_system_flags)。

# 参考链接

- [Node.js官方文档：fs模块](https://nodejs.org/api/fs.html)
- [Node.js官方文档：fs.access()方法](https://nodejs.org/api/fs.html#fs_fs_access_path_mode_callback)