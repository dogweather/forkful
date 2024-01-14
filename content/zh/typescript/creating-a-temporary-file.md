---
title:    "TypeScript: 创建临时文件"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

为什么：为了帮助我们处理临时文件，我们经常需要创建临时文件，这样我们就可以将数据暂时存储在一个临时位置，然后在处理完成后再进行删除。

如何创建：在TypeScript中，我们可以使用fs模块来创建临时文件。首先，我们需要在我们的代码中导入fs模块，然后使用fs.openSync()方法来创建一个临时文件。接着，我们就可以使用fs.writeFile()方法来写入数据到该临时文件中。

```TypeScript
import fs from 'fs';

// Create a temporary file
const tempFile = fs.openSync('tempFile.txt', 'w+');

// Write data to the file
fs.writeFile(tempFile, 'This is a temporary file', (err) => {
    if (err) throw err;
    console.log('Data successfully written to temporary file.');
});

// Close the file
fs.closeSync(tempFile);

// Output:
// Data successfully written to temporary file.
```

深入探讨：当我们创建临时文件时，我们可以为文件指定一个文件名，也可以让操作系统自动生成一个随机的文件名。此外，我们还可以指定临时文件的存储路径和文件的权限等信息。在处理完成后，我们也可以使用fs.unlink()方法来删除临时文件。

另外，我们还可以使用操作系统提供的临时文件夹来创建临时文件。这些临时文件夹通常位于操作系统的默认临时目录中，并会在系统重启后自动清空。在Node.js中，我们可以使用os.tmpdir()方法来获取操作系统的临时文件夹路径，并将此路径添加到临时文件的文件名中，就可以创建一个存放在系统临时文件夹中的临时文件。

```TypeScript
import fs from 'fs';
import os from 'os';

// Create a temporary file in the operating system's default temporary directory
const tempFolder = os.tmpdir();
const tempFile = fs.openSync(`${tempFolder}/tempFile.txt`, 'w+');

// Write data to the file
fs.writeFile(tempFile, 'This is a temporary file', (err) => {
    if (err) throw err;
    console.log('Data successfully written to temporary file.');
});

// Close the file
fs.closeSync(tempFile);

// Output:
// Data successfully written to temporary file.
```

请点击这里查看更多关于创建临时文件的信息和代码示例：https://nodejs.org/api/fs.html#fs_fs_open_path_flags_mode_callback

请参阅：https://nodejs.org/api/os.html#os_os_tmpdir

请参阅更多关于Node.js中fs模块的使用方法和实例：https://www.tutorialsteacher.com/nodejs/nodejs-file-system

### 技术文章其他链接：

- [TypeScript官方文档](https://www.typescriptlang.org/docs/)
- [Node.js官方文档](https://nodejs.org/en/docs/)