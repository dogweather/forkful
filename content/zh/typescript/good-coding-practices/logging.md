---
date: 2024-01-26 01:09:09.798454-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728TypeScript\u4E2D\uFF0C\u4F60\u53EF\
  \u4EE5\u4F7F\u7528\u63A7\u5236\u53F0\uFF08console\uFF09\u65B9\u6CD5\u5F88\u5BB9\u6613\
  \u5730\u5B9E\u73B0\u57FA\u672C\u65E5\u5FD7\u8BB0\u5F55\uFF0C\u6216\u8005\u96C6\u6210\
  \u66F4\u9AD8\u7EA7\u7684\u65E5\u5FD7\u529F\u80FD\uFF0C\u6BD4\u5982\u4F7F\u7528`winston`\u6216\
  `pino`\u7B49\u5E93\u3002\u8FD9\u91CC\u6709\u4E00\u4E2A\u4F7F\u7528`console.log`\u7684\
  \u57FA\u672C\u793A\u4F8B\uFF0C\u4EE5\u53CA\u4E00\u4E2A\u4F7F\u7528`winston`\u7684\
  \u66F4\u9AD8\u7EA7\u793A\u4F8B\u3002"
lastmod: '2024-03-13T22:44:47.479066-06:00'
model: gpt-4-1106-preview
summary: "\u5728TypeScript\u4E2D\uFF0C\u4F60\u53EF\u4EE5\u4F7F\u7528\u63A7\u5236\u53F0\
  \uFF08console\uFF09\u65B9\u6CD5\u5F88\u5BB9\u6613\u5730\u5B9E\u73B0\u57FA\u672C\u65E5\
  \u5FD7\u8BB0\u5F55\uFF0C\u6216\u8005\u96C6\u6210\u66F4\u9AD8\u7EA7\u7684\u65E5\u5FD7\
  \u529F\u80FD\uFF0C\u6BD4\u5982\u4F7F\u7528`winston`\u6216`pino`\u7B49\u5E93\u3002\
  \u8FD9\u91CC\u6709\u4E00\u4E2A\u4F7F\u7528`console.log`\u7684\u57FA\u672C\u793A\u4F8B\
  \uFF0C\u4EE5\u53CA\u4E00\u4E2A\u4F7F\u7528`winston`\u7684\u66F4\u9AD8\u7EA7\u793A\
  \u4F8B."
title: "\u65E5\u5FD7\u8BB0\u5F55"
weight: 17
---

## 如何操作：
在TypeScript中，你可以使用控制台（console）方法很容易地实现基本日志记录，或者集成更高级的日志功能，比如使用`winston`或`pino`等库。这里有一个使用`console.log`的基本示例，以及一个使用`winston`的更高级示例。

```TypeScript
// 基本的控制台日志记录
console.log('信息：启动应用程序...');
console.error('错误：无法检索数据。');

// 示例输出
// 信息：启动应用程序...
// 错误：无法检索数据。
```

为了实现更健壮的日志记录，让我们设置`winston`：

```TypeScript
import { createLogger, format, transports } from 'winston';

const logger = createLogger({
  level: 'info',
  format: format.combine(
    format.timestamp({ format: 'YYYY-MM-DD HH:mm:ss' }),
    format.printf(info => `${info.timestamp} ${info.level}: ${info.message}`)
  ),
  transports: [
    new transports.Console(),
    new transports.File({ filename: 'combined.log' })
  ]
});

logger.info('服务器启动！');
logger.warn('硬盘空间不足警告。');
logger.error('无法连接到数据库。');

// combined.log 文件中的示例输出
// 2023-01-20 14:42:07 信息：服务器启动！
// 2023-01-20 14:42:09 警告：硬盘空间不足。
// 2023-01-20 14:42:12 错误：无法连接到数据库。
```

## 深入了解：
在计算的背景下，日志概念可以追溯到编程的早期时代，其术语本身来源于“航海日志”，这是一种航海记录系统。在历史上，程序事件经常被记录到物理打印输出或终端输出上，特别是在大型机时代。

快速发展到今天，你可以使用许多不同的工具和库，以满足各种日志记录需求，从简单的文本文件到复杂的日志管理系统。除了`winston`以外的其他选择还包括`pino`，其以高性能自豪，以及基于JSON的`Bunyan`。在使用Node.js时，日志库经常提供流机制，将日志导向不同的目的地，支持日志轮换，并提供自定义格式化器。

在实现方面，日志消息通常包含时间戳、严重性级别（如信息、警告、错误）以及实际消息。良好的日志实践建议适当地分类日志级别，避免在日志中包含敏感数据，并在高吞吐量应用程序中考虑性能影响。

## 另见：
- [Winston - 几乎适用于所有的日志记录器](https://www.npmjs.com/package/winston)
- [Pino - 非常低开销的Node.js日志记录器](https://www.npmjs.com/package/pino)
- [Node.js日志记录最佳实践](https://thisdavej.com/using-winston-a-versatile-logging-library-for-node-js/)
- [The 12 Factor App - 日志](https://12factor.net/logs)
