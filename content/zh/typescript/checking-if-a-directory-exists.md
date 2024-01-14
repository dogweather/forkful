---
title:                "TypeScript: 判断目录是否存在"
simple_title:         "判断目录是否存在"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 为什么要检查目录是否存在

当我们进行文件操作时，经常会涉及到检查目录是否存在的情况。这是因为我们需要确保我们的代码能够正常工作，而不会受到缺少目录的影响。因此，检查目录是否存在可以使我们的代码更加安全和稳定。

## 如何检查目录是否存在

要检查目录是否存在，我们可以使用TypeScript的内置函数`fs.existsSync()`。这个函数会返回一个布尔值，如果目录存在则为true，反之为false。让我们来看一个例子：

```TypeScript
import fs from "fs";

const directoryPath = "./example_dir";
if(fs.existsSync(directoryPath)) {
    console.log("目录存在");
} else {
    console.log("目录不存在");
}
```

运行这段代码，如果`example_dir`目录存在，则会输出"目录存在"，否则会输出"目录不存在"。

## 深入了解检查目录是否存在

除了使用`fs.existsSync()`外，我们还可以使用`fs.statSync()`来检查目录是否存在。`fs.statSync()`会返回一个对象，包含目录的详细信息。如果目录不存在，则会抛出一个错误。让我们看一个例子：

```TypeScript
import fs from "fs";

const directoryPath = "./example_dir";
try {
    fs.statSync(directoryPath);
    console.log("目录存在");
} catch (err) {
    console.log("目录不存在");
}
```

这个例子中，我们使用了`try...catch`语句来捕获可能抛出的错误，并在目录不存在时输出"目录不存在"。

## 参考链接

- 关于`fs.existsSync()`的官方文档：https://nodejs.org/api/fs.html#fs_fs_existssync_path
- 关于`fs.statSync()`的官方文档：https://nodejs.org/api/fs.html#fs_fs_statsync_path_options