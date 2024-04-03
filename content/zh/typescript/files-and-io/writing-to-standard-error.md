---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:42.375562-07:00
description: "\u5982\u4F55\u505A\uFF1A TypeScript\u4F5C\u4E3AJavaScript\u7684\u8D85\
  \u96C6\uFF0C\u4F9D\u8D56\u4E8E\u5E95\u5C42JS\u8FD0\u884C\u65F6\u73AF\u5883\uFF08\
  \u5982Node.js\uFF09\u6765\u5B9E\u73B0\u5411stderr\u5199\u5165\u3002\u4EE5\u4E0B\u662F\
  \u76F4\u63A5\u505A\u5230\u8FD9\u4E00\u70B9\u7684\u65B9\u6CD5\uFF1A."
lastmod: '2024-03-13T22:44:47.490499-06:00'
model: gpt-4-0125-preview
summary: "TypeScript\u4F5C\u4E3AJavaScript\u7684\u8D85\u96C6\uFF0C\u4F9D\u8D56\u4E8E\
  \u5E95\u5C42JS\u8FD0\u884C\u65F6\u73AF\u5883\uFF08\u5982Node.js\uFF09\u6765\u5B9E\
  \u73B0\u5411stderr\u5199\u5165\u3002\u4EE5\u4E0B\u662F\u76F4\u63A5\u505A\u5230\u8FD9\
  \u4E00\u70B9\u7684\u65B9\u6CD5\uFF1A."
title: "\u5199\u5165\u6807\u51C6\u9519\u8BEF"
weight: 25
---

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
