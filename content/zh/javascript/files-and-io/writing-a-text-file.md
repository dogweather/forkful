---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:23.436836-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728 Node.js \u73AF\u5883\u4E2D\uFF0C\
  \u4F60\u53EF\u4EE5\u4F7F\u7528\u5185\u7F6E\u7684 `fs`\uFF08\u6587\u4EF6\u7CFB\u7EDF\
  \uFF09\u6A21\u5757\u6765\u5199\u5165\u6587\u672C\u6587\u4EF6\u3002\u6B64\u793A\u4F8B\
  \u5C55\u793A\u4E86\u5982\u4F55\u5F02\u6B65\u5730\u5C06\u6587\u672C\u5199\u5165\u6587\
  \u4EF6\uFF1A."
lastmod: '2024-03-13T22:44:48.236340-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Node.js \u73AF\u5883\u4E2D\uFF0C\u4F60\u53EF\u4EE5\u4F7F\u7528\u5185\
  \u7F6E\u7684 `fs`\uFF08\u6587\u4EF6\u7CFB\u7EDF\uFF09\u6A21\u5757\u6765\u5199\u5165\
  \u6587\u672C\u6587\u4EF6\u3002\u6B64\u793A\u4F8B\u5C55\u793A\u4E86\u5982\u4F55\u5F02\
  \u6B65\u5730\u5C06\u6587\u672C\u5199\u5165\u6587\u4EF6\uFF1A."
title: "\u7F16\u5199\u6587\u672C\u6587\u4EF6"
weight: 24
---

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
