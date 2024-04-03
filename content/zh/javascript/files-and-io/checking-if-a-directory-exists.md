---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:39.617096-07:00
description: "\u5728JavaScript\u4E2D\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728\
  \u5BF9\u4E8E\u6587\u4EF6\u64CD\u4F5C\u4EFB\u52A1\u81F3\u5173\u91CD\u8981\uFF0C\u5B83\
  \u4F7F\u5F97\u811A\u672C\u5728\u8BFB\u53D6\u6216\u5199\u5165\u76EE\u5F55\u4E4B\u524D\
  \u80FD\u591F\u9A8C\u8BC1\u76EE\u5F55\u662F\u5426\u5B58\u5728\u3002\u8FD9\u4E00\u64CD\
  \u4F5C\u53EF\u4EE5\u9884\u9632\u9519\u8BEF\u5E76\u786E\u4FDD\u7A0B\u5E8F\u66F4\u52A0\
  \u987A\u7545\u5730\u6267\u884C\uFF0C\u7279\u522B\u662F\u5728\u90A3\u4E9B\u57FA\u4E8E\
  \u7528\u6237\u8F93\u5165\u6216\u5916\u90E8\u6570\u636E\u6E90\u52A8\u6001\u5904\u7406\
  \u6587\u4EF6\u6216\u76EE\u5F55\u7684\u5E94\u7528\u4E2D\u3002"
lastmod: '2024-03-13T22:44:48.231469-06:00'
model: gpt-4-0125-preview
summary: "\u5728JavaScript\u4E2D\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728\u5BF9\
  \u4E8E\u6587\u4EF6\u64CD\u4F5C\u4EFB\u52A1\u81F3\u5173\u91CD\u8981\uFF0C\u5B83\u4F7F\
  \u5F97\u811A\u672C\u5728\u8BFB\u53D6\u6216\u5199\u5165\u76EE\u5F55\u4E4B\u524D\u80FD\
  \u591F\u9A8C\u8BC1\u76EE\u5F55\u662F\u5426\u5B58\u5728\u3002\u8FD9\u4E00\u64CD\u4F5C\
  \u53EF\u4EE5\u9884\u9632\u9519\u8BEF\u5E76\u786E\u4FDD\u7A0B\u5E8F\u66F4\u52A0\u987A\
  \u7545\u5730\u6267\u884C\uFF0C\u7279\u522B\u662F\u5728\u90A3\u4E9B\u57FA\u4E8E\u7528\
  \u6237\u8F93\u5165\u6216\u5916\u90E8\u6570\u636E\u6E90\u52A8\u6001\u5904\u7406\u6587\
  \u4EF6\u6216\u76EE\u5F55\u7684\u5E94\u7528\u4E2D\u3002."
title: "\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728"
weight: 20
---

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
