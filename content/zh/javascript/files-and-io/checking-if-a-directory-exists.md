---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:39.617096-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728Node.js\u4E2D\uFF0C\u7531\u4E8E\
  JavaScript\u672C\u8EAB\u4E0D\u80FD\u76F4\u63A5\u8BBF\u95EE\u6587\u4EF6\u7CFB\u7EDF\
  \uFF0C\u56E0\u6B64\u901A\u5E38\u4F7F\u7528`fs`\u6A21\u5757\u8FDB\u884C\u6B64\u7C7B\
  \u64CD\u4F5C\u3002\u4E0B\u9762\u662F\u4F7F\u7528`fs.existsSync()`\u68C0\u67E5\u76EE\
  \u5F55\u662F\u5426\u5B58\u5728\u7684\u7B80\u5355\u65B9\u5F0F\uFF1A."
lastmod: '2024-03-13T22:44:48.231469-06:00'
model: gpt-4-0125-preview
summary: "\u5728Node.js\u4E2D\uFF0C\u7531\u4E8EJavaScript\u672C\u8EAB\u4E0D\u80FD\u76F4\
  \u63A5\u8BBF\u95EE\u6587\u4EF6\u7CFB\u7EDF\uFF0C\u56E0\u6B64\u901A\u5E38\u4F7F\u7528\
  `fs`\u6A21\u5757\u8FDB\u884C\u6B64\u7C7B\u64CD\u4F5C\u3002\u4E0B\u9762\u662F\u4F7F\
  \u7528`fs.existsSync()`\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728\u7684\u7B80\
  \u5355\u65B9\u5F0F\uFF1A."
title: "\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728"
weight: 20
---

## 如何操作：
在Node.js中，由于JavaScript本身不能直接访问文件系统，因此通常使用`fs`模块进行此类操作。下面是使用`fs.existsSync()`检查目录是否存在的简单方式：

```javascript
const fs = require('fs');

const directoryPath = './sample-directory';

// 检查目录是否存在
if (fs.existsSync(directoryPath)) {
  console.log('目录存在。');
} else {
  console.log('目录不存在。');
}
```
**示例输出：**
```
目录存在。
```
或者，对于非阻塞的异步方法，使用`fs.promises`配合`async/await`：

```javascript
const fs = require('fs').promises;

async function checkDirectory(directoryPath) {
  try {
    await fs.access(directoryPath);
    console.log('目录存在。');
  } catch (error) {
    console.log('目录不存在。');
  }
}

checkDirectory('./sample-directory');
```
**示例输出：**
```
目录存在。
```

对于大量使用文件和目录操作的项目，`fs-extra`包作为原生`fs`模块的扩展，提供了方便的额外方法。以下是如何使用`fs-extra`达到相同的目的：

```javascript
const fs = require('fs-extra');

const directoryPath = './sample-directory';

// 检查目录是否存在
fs.pathExists(directoryPath)
  .then(exists => console.log(exists ? '目录存在。' : '目录不存在。'))
  .catch(err => console.error(err));
```
**示例输出：**
```
目录存在。
```

这种方法使得代码清晰易读，并且能够无缝整合现代JavaScript实践。
