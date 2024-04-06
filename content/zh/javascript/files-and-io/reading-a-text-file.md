---
date: 2024-01-20 17:54:56.819171-07:00
description: "How to: \u5982\u4F55\u505A\uFF1A \u65E9\u671F\uFF0C\u8BFB\u53D6\u6587\
  \u4EF6\u901A\u5E38\u4F9D\u8D56\u4E8E\u64CD\u4F5C\u7CFB\u7EDF\u7EA7\u522B\u7684API\u8C03\
  \u7528\u3002\u73B0\u5728\uFF0CNode.js\u901A\u8FC7`fs`\u6A21\u5757\u63D0\u4F9B\u8FD9\
  \u4E9B\u529F\u80FD\uFF0C\u5BF9\u5F02\u6B65IO\u8FDB\u884C\u4E86\u5C01\u88C5\u3002\
  \u9664\u4E86`readFile`\uFF0C\u8FD8\u6709`readFileSync`\u65B9\u6CD5\u540C\u6B65\u8BFB\
  \u53D6\u6587\u4EF6\uFF0C\u4F46\u4F1A\u963B\u585E\u3002\u5728\u751F\u4EA7\u73AF\u5883\
  \uFF0C\u5F02\u6B65\u7248\u672C\u66F4\u5E38\u7528\u3002\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:01.431537-06:00'
model: gpt-4-1106-preview
summary: "\u5982\u4F55\u505A\uFF1A \u65E9\u671F\uFF0C\u8BFB\u53D6\u6587\u4EF6\u901A\
  \u5E38\u4F9D\u8D56\u4E8E\u64CD\u4F5C\u7CFB\u7EDF\u7EA7\u522B\u7684API\u8C03\u7528\
  \u3002\u73B0\u5728\uFF0CNode.js\u901A\u8FC7`fs`\u6A21\u5757\u63D0\u4F9B\u8FD9\u4E9B\
  \u529F\u80FD\uFF0C\u5BF9\u5F02\u6B65IO\u8FDB\u884C\u4E86\u5C01\u88C5\u3002\u9664\
  \u4E86`readFile`\uFF0C\u8FD8\u6709`readFileSync`\u65B9\u6CD5\u540C\u6B65\u8BFB\u53D6\
  \u6587\u4EF6\uFF0C\u4F46\u4F1A\u963B\u585E\u3002\u5728\u751F\u4EA7\u73AF\u5883\uFF0C\
  \u5F02\u6B65\u7248\u672C\u66F4\u5E38\u7528\u3002"
title: "\u9605\u8BFB\u6587\u672C\u6587\u4EF6"
weight: 22
---

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
