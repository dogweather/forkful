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

#「什么 & 为什么？」
检查一个目录是否存在是指检查电脑上是否已经存在一个具有特定名称的文件夹。程序员通常会这样做是为了避免重复创建同名的文件夹，也可以在程序运行时检查文件夹是否存在来决定下一步的操作。

#「如何：」
```
TypeScript 
import * as fs from 'fs';

const directoryPath = '../directory_name';

fs.stat(directoryPath, function (err) {
    if (err === null) {
        console.log('Directory exists.');
    } else if (err.code === 'ENOENT') {
        console.log('Directory does not exist.');
    } else {
        console.log('Error: ', err);
    }
});
```

#「深入浅出」
1. 历史背景：在早期的操作系统中，程序员必须手动检查是否存在某个文件夹来确定下一步的操作，随着操作系统的发展，提供了更加便捷的 API 来检查文件夹的存在性。

2. 替代方案：除了使用 fs 模块中的 `fs.stat()` 方法之外，还可以使用 `fs.existsSync()` 方法来检查文件夹是否存在，并返回一个 `boolean` 值。

3. 实现细节：fs 模块是 Node.js 中用于处理文件系统操作的核心模块，可以通过 `require('fs')` 来引入并使用其中的方法。

#「相关链接」
- Node.js fs 模块文档：https://nodejs.org/api/fs.html
- TypeScript 官方文档：https://www.typescriptlang.org/