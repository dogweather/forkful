---
date: 2024-01-20 17:55:15.620596-07:00
description: "\u8BFB\u53D6\u6587\u672C\u6587\u4EF6\u5C31\u662F\u4ECE\u6587\u4EF6\u4E2D\
  \u63D0\u53D6\u6570\u636E\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\
  \u5904\u7406\u3001\u5206\u6790\u6216\u663E\u793A\u5B58\u50A8\u5728\u6587\u4EF6\u4E2D\
  \u7684\u4FE1\u606F\u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:06.508203
model: gpt-4-1106-preview
summary: "\u8BFB\u53D6\u6587\u672C\u6587\u4EF6\u5C31\u662F\u4ECE\u6587\u4EF6\u4E2D\
  \u63D0\u53D6\u6570\u636E\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\
  \u5904\u7406\u3001\u5206\u6790\u6216\u663E\u793A\u5B58\u50A8\u5728\u6587\u4EF6\u4E2D\
  \u7684\u4FE1\u606F\u3002"
title: "\u9605\u8BFB\u6587\u672C\u6587\u4EF6"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
读取文本文件就是从文件中提取数据。程序员这么做是为了处理、分析或显示存储在文件中的信息。

## How to (如何操作)
使用Node.js的`fs`模块和TypeScript来读取文件。需要有`@types/node`包支持类型。

```typescript
import { readFile } from 'fs/promises';

async function readTextFile(filePath: string): Promise<string> {
    try {
        const data = await readFile(filePath, 'utf8');
        console.log(data);
        return data;
    } catch (err) {
        console.error('Error reading file:', err);
        throw err;
    }
}

// 使用示例
const filePath = './example.txt';

readTextFile(filePath).then((content) => {
    console.log('File content:', content);
});
```

示例输出：

```
File content: 这是文件内容的示例。
```

## Deep Dive (深入了解)
历史背景：Node.js 从一开始就提供了文件操作API。TypeScript随后增加了类型检查和更现代的异步处理模式。

替代方案：除了`fs/promises`，使用`fs.readFileSync`和`fs.readFile`也可以读取文件，但这会导致不同的代码风格和性能考量。

实现细节：`readFile`是异步的，返回`Promise`。它使用UTF-8编码读取文件内容，保持了灵活性和通用性。

## See Also (另请参阅)
- Node.js `fs`模块官方文档: https://nodejs.org/api/fs.html
- TypeScript官方手册: https://www.typescriptlang.org/docs/
- `@types/node`NPM包详情: https://www.npmjs.com/package/@types/node
