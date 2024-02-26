---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:42.375562-07:00
description: "\u5728TypeScript\u4E2D\uFF0C\u5411\u6807\u51C6\u9519\u8BEF\uFF08stderr\uFF09\
  \u5199\u5165\u662F\u5C06\u9519\u8BEF\u6D88\u606F\u6216\u65E5\u5FD7\u76F4\u63A5\u53D1\
  \u9001\u5230\u73AF\u5883\u7684\u9519\u8BEF\u8F93\u51FA\u6D41\u7684\u8FC7\u7A0B\uFF08\
  \u4F8B\u5982\uFF0Cnode.js\u6216Web\u6D4F\u89C8\u5668\u4E2D\u7684\u63A7\u5236\u53F0\
  \uFF09\u3002\u8FD9\u5BF9\u4E8E\u5728\u4E0D\u5E72\u6270\u901A\u5E38\u7528\u4E8E\u7A0B\
  \u5E8F\u6570\u636E\u7684\u6807\u51C6\u8F93\u51FA\uFF08stdout\uFF09\u7684\u60C5\u51B5\
  \u4E0B\u8BCA\u65AD\u95EE\u9898\u81F3\u5173\u91CD\u8981\uFF0C\u786E\u4FDD\u9519\u8BEF\
  \u5904\u7406\u548C\u65E5\u5FD7\u8BB0\u5F55\u5F97\u5230\u9AD8\u6548\u3001\u7EDF\u4E00\
  \u7684\u7BA1\u7406\u3002"
lastmod: '2024-02-25T18:49:45.053364-07:00'
model: gpt-4-0125-preview
summary: "\u5728TypeScript\u4E2D\uFF0C\u5411\u6807\u51C6\u9519\u8BEF\uFF08stderr\uFF09\
  \u5199\u5165\u662F\u5C06\u9519\u8BEF\u6D88\u606F\u6216\u65E5\u5FD7\u76F4\u63A5\u53D1\
  \u9001\u5230\u73AF\u5883\u7684\u9519\u8BEF\u8F93\u51FA\u6D41\u7684\u8FC7\u7A0B\uFF08\
  \u4F8B\u5982\uFF0Cnode.js\u6216Web\u6D4F\u89C8\u5668\u4E2D\u7684\u63A7\u5236\u53F0\
  \uFF09\u3002\u8FD9\u5BF9\u4E8E\u5728\u4E0D\u5E72\u6270\u901A\u5E38\u7528\u4E8E\u7A0B\
  \u5E8F\u6570\u636E\u7684\u6807\u51C6\u8F93\u51FA\uFF08stdout\uFF09\u7684\u60C5\u51B5\
  \u4E0B\u8BCA\u65AD\u95EE\u9898\u81F3\u5173\u91CD\u8981\uFF0C\u786E\u4FDD\u9519\u8BEF\
  \u5904\u7406\u548C\u65E5\u5FD7\u8BB0\u5F55\u5F97\u5230\u9AD8\u6548\u3001\u7EDF\u4E00\
  \u7684\u7BA1\u7406\u3002"
title: "\u5199\u5165\u6807\u51C6\u9519\u8BEF"
---

{{< edit_this_page >}}

## 什么 & 为什么？
在TypeScript中，向标准错误（stderr）写入是将错误消息或日志直接发送到环境的错误输出流的过程（例如，node.js或Web浏览器中的控制台）。这对于在不干扰通常用于程序数据的标准输出（stdout）的情况下诊断问题至关重要，确保错误处理和日志记录得到高效、统一的管理。

## 如何做：
TypeScript作为JavaScript的超集，依赖于底层JS运行时环境（如Node.js）来实现向stderr写入。以下是直接做到这一点的方法：

```typescript
console.error("这是一个错误消息。");
```

stderr的样本输出：
```
这是一个错误消息。
```

在Node.js环境中，您还可以使用`process.stderr.write()`方法进行更低级别的写入：

```typescript
process.stderr.write("低级别错误消息。\n");
```

stderr的样本输出：
```
低级别错误消息。
```

对于更结构化的错误日志，您可能会使用如`winston`或`pino`等流行的第三方库。以下是使用`winston`记录错误的方法：

首先，安装`winston`：

```bash
npm install winston
```

然后在您的TypeScript文件中使用它：

```typescript
import * as winston from 'winston';

const logger = winston.createLogger({
  levels: winston.config.syslog.levels,
  transports: [
    new winston.transports.Console(),
    new winston.transports.File({ filename: 'error.log', level: 'error' })
  ],
});

logger.error('使用winston记录的错误。');
```

这将把错误同时写入控制台和一个名为`error.log`的文件。记住，写入文件时，管理文件权限和滚动以防止与磁盘空间使用有关的问题是很重要的。
