---
date: 2024-01-20 17:54:56.819171-07:00
description: "\u8BFB\u53D6\u6587\u672C\u6587\u4EF6\u5C31\u662F\u628A\u786C\u76D8\u4E0A\
  \u7684\u6587\u672C\u5185\u5BB9\u8F7D\u5165\u5230\u7A0B\u5E8F\u4E2D\u3002\u7A0B\u5E8F\
  \u5458\u8FD9\u4E48\u505A\u53EF\u4EE5\u5904\u7406\u6570\u636E\u3001\u914D\u7F6E\u4FE1\
  \u606F\u6216\u8005\u4ECE\u5916\u90E8\u6E90\u8BFB\u53D6\u8F93\u5165\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:48.235032-06:00'
model: gpt-4-1106-preview
summary: "\u8BFB\u53D6\u6587\u672C\u6587\u4EF6\u5C31\u662F\u628A\u786C\u76D8\u4E0A\
  \u7684\u6587\u672C\u5185\u5BB9\u8F7D\u5165\u5230\u7A0B\u5E8F\u4E2D\u3002\u7A0B\u5E8F\
  \u5458\u8FD9\u4E48\u505A\u53EF\u4EE5\u5904\u7406\u6570\u636E\u3001\u914D\u7F6E\u4FE1\
  \u606F\u6216\u8005\u4ECE\u5916\u90E8\u6E90\u8BFB\u53D6\u8F93\u5165\u3002"
title: "\u9605\u8BFB\u6587\u672C\u6587\u4EF6"
weight: 22
---

## What & Why? 什么和为什么？
读取文本文件就是把硬盘上的文本内容载入到程序中。程序员这么做可以处理数据、配置信息或者从外部源读取输入。

## How to: 如何做：
使用Node.js读取文件，简单、直接。

```javascript
const fs = require('fs');

fs.readFile('example.txt', 'utf8' , (err, data) => {
  if (err) {
    console.error(err);
    return;
  }
  console.log(data);
});
```
输出：

```
这是文本文件的内容！
```

## Deep Dive 深入研究：
早期，读取文件通常依赖于操作系统级别的API调用。现在，Node.js通过`fs`模块提供这些功能，对异步IO进行了封装。除了`readFile`，还有`readFileSync`方法同步读取文件，但会阻塞。在生产环境，异步版本更常用。

流（Streams）是另一种读取文件的方法，适用于读取大文件。它们分批读取内容，节省内存。

```javascript
const fs = require('fs');
const stream = fs.createReadStream('largeFile.txt', 'utf8');

stream.on('data', function(chunk) {
    console.log(chunk);
});

stream.on('error', function(err) {
    console.log(err);
});

stream.on('end', function() {
    console.log('Finished reading file');
});
```

## See Also 另请参阅：
- Node.js `fs`模块官方文档：[Node.js File System](https://nodejs.org/api/fs.html)
- MDN关于JavaScript的工作原理：[JavaScript MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript)
- 关于Node.js Stream的官方文档：[Node.js Stream](https://nodejs.org/api/stream.html)
