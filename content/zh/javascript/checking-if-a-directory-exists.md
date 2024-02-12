---
title:                "检查目录是否存在"
aliases:
- zh/javascript/checking-if-a-directory-exists.md
date:                  2024-02-03T19:07:39.617096-07:00
model:                 gpt-4-0125-preview
simple_title:         "检查目录是否存在"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么和为什么？
在JavaScript中检查目录是否存在对于文件操作任务至关重要，它使得脚本在读取或写入目录之前能够验证目录是否存在。这一操作可以预防错误并确保程序更加顺畅地执行，特别是在那些基于用户输入或外部数据源动态处理文件或目录的应用中。

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
