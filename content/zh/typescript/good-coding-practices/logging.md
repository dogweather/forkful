---
date: 2024-01-26 01:09:09.798454-07:00
description: "\u65E5\u5FD7\u8BB0\u5F55\u662F\u5728\u7A0B\u5E8F\u6267\u884C\u8FC7\u7A0B\
  \u4E2D\u5C06\u4E8B\u4EF6\u3001\u9519\u8BEF\u548C\u5176\u4ED6\u91CD\u8981\u4FE1\u606F\
  \u8BB0\u5F55\u5230\u5916\u90E8\u4ECB\u8D28\u4E0A\uFF0C\u7ECF\u5E38\u662F\u6587\u4EF6\
  \u6216\u6570\u636E\u5E93\u3002\u7A0B\u5E8F\u5458\u4F7F\u7528\u65E5\u5FD7\u6765\u76D1\
  \u63A7\u8F6F\u4EF6\u884C\u4E3A\uFF0C\u8C03\u8BD5\u95EE\u9898\uFF0C\u4EE5\u53CA\u8DDF\
  \u8E2A\u7CFB\u7EDF\u6D3B\u52A8\u7528\u4E8E\u5B89\u5168\u548C\u6027\u80FD\u5206\u6790\
  \u3002"
lastmod: '2024-03-11T00:14:21.241277-06:00'
model: gpt-4-1106-preview
summary: "\u65E5\u5FD7\u8BB0\u5F55\u662F\u5728\u7A0B\u5E8F\u6267\u884C\u8FC7\u7A0B\
  \u4E2D\u5C06\u4E8B\u4EF6\u3001\u9519\u8BEF\u548C\u5176\u4ED6\u91CD\u8981\u4FE1\u606F\
  \u8BB0\u5F55\u5230\u5916\u90E8\u4ECB\u8D28\u4E0A\uFF0C\u7ECF\u5E38\u662F\u6587\u4EF6\
  \u6216\u6570\u636E\u5E93\u3002\u7A0B\u5E8F\u5458\u4F7F\u7528\u65E5\u5FD7\u6765\u76D1\
  \u63A7\u8F6F\u4EF6\u884C\u4E3A\uFF0C\u8C03\u8BD5\u95EE\u9898\uFF0C\u4EE5\u53CA\u8DDF\
  \u8E2A\u7CFB\u7EDF\u6D3B\u52A8\u7528\u4E8E\u5B89\u5168\u548C\u6027\u80FD\u5206\u6790\
  \u3002"
title: "\u65E5\u5FD7\u8BB0\u5F55"
---

{{< edit_this_page >}}

## 什么是日志？以及为什么要用？

日志记录是在程序执行过程中将事件、错误和其他重要信息记录到外部介质上，经常是文件或数据库。程序员使用日志来监控软件行为，调试问题，以及跟踪系统活动用于安全和性能分析。

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
