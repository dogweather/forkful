---
title:                "写入标准错误"
aliases:
- zh/typescript/writing-to-standard-error.md
date:                  2024-02-03T19:34:42.375562-07:00
model:                 gpt-4-0125-preview
simple_title:         "写入标准错误"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
