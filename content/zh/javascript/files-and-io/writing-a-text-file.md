---
title:                "编写文本文件"
aliases:
- /zh/javascript/writing-a-text-file.md
date:                  2024-02-03T19:28:23.436836-07:00
model:                 gpt-4-0125-preview
simple_title:         "编写文本文件"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/writing-a-text-file.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么及为什么？
在 JavaScript 中写入文本文件通常涉及创建并保存数据于一个简单、可读的格式中，用于日志记录、导出用户输入或配置目的。这一功能对于需要将数据持久化超出应用程序进程生命周期的应用程序至关重要，提供了一种存储以及之后检索或分享信息的方式。

## 如何操作：
在 Node.js 环境中，你可以使用内置的 `fs`（文件系统）模块来写入文本文件。此示例展示了如何异步地将文本写入文件：

```javascript
const fs = require('fs');

const data = 'Hello, World! 这是要写入文件的文本。';

fs.writeFile('example.txt', data, (err) => {
  if (err) {
    throw err;
  }
  console.log('文件已被写入。');
});
```

示例输出：
```
文件已被写入。
```

对于同步写文件，使用 `writeFileSync`：
```javascript
try {
  fs.writeFileSync('example.txt', data);
  console.log('文件已被写入。');
} catch (error) {
  console.error('写文件错误：', error);
}
```

在现代网页浏览器中，文件系统访问 API 引入了读写文件的能力。然而，其使用受到用户权限的制约。这里是如何创建并写入文件的方法：

```javascript
if ('showSaveFilePicker' in window) {
  const handle = await window.showSaveFilePicker();
  const writable = await handle.createWritable();
  await writable.write('Hello, World! 这是浏览器文本文件写入。');
  await writable.close();
}
```

对于更复杂的场景或在处理大文件时，你可能会选择使用第三方库，如浏览器中的 `FileSaver.js`：

```html
<script src="https://cdnjs.cloudflare.com/ajax/libs/FileSaver.js/2.0.2/FileSaver.min.js"></script>
<script>
  const blob = new Blob(["Hello, World! 这是来自 FileSaver.js 的文本。"], {type: "text/plain;charset=utf-8"});
  saveAs(blob, "example.txt");
</script>
```

记住，在客户端（浏览器中）写文件由于安全问题是受限的，任何需要保存到用户本地磁盘的操作通常都需要他们的明确许可。
