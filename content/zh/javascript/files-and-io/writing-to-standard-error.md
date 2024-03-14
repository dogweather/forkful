---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:43.615065-07:00
description: "\u5728JavaScript\u4E2D\u5199\u5165\u6807\u51C6\u9519\u8BEF\uFF08stderr\uFF09\
  \u662F\u6307\u5C06\u9519\u8BEF\u6D88\u606F\u6216\u4EFB\u4F55\u5173\u952E\u4FE1\u606F\
  \u5F15\u5BFC\u81F3\u4E00\u4E2A\u7279\u5B9A\u7684\u3001\u72EC\u7ACB\u7684\u6D41\u4E2D\
  \uFF0C\u8FD9\u5728Unix\u7C7B\u73AF\u5883\u4E2D\u5BF9\u4E8E\u65E5\u5FD7\u8BB0\u5F55\
  \u548C\u8C03\u8BD5\u76EE\u7684\u5C24\u5176\u6709\u7528\u3002\u7A0B\u5E8F\u5458\u8FD9\
  \u6837\u505A\u662F\u4E3A\u4E86\u5C06\u6B63\u5E38\u7A0B\u5E8F\u8F93\u51FA\u4E0E\u9519\
  \u8BEF\u6D88\u606F\u533A\u5206\u5F00\uFF0C\u4ECE\u800C\u5B9E\u73B0\u66F4\u6E05\u6670\
  \u7684\u8F93\u51FA\u7BA1\u7406\u548C\u66F4\u5BB9\u6613\u7684\u9519\u8BEF\u76D1\u63A7\
  \u3002"
lastmod: '2024-03-13T22:44:48.233930-06:00'
model: gpt-4-0125-preview
summary: "\u5728JavaScript\u4E2D\u5199\u5165\u6807\u51C6\u9519\u8BEF\uFF08stderr\uFF09\
  \u662F\u6307\u5C06\u9519\u8BEF\u6D88\u606F\u6216\u4EFB\u4F55\u5173\u952E\u4FE1\u606F\
  \u5F15\u5BFC\u81F3\u4E00\u4E2A\u7279\u5B9A\u7684\u3001\u72EC\u7ACB\u7684\u6D41\u4E2D\
  \uFF0C\u8FD9\u5728Unix\u7C7B\u73AF\u5883\u4E2D\u5BF9\u4E8E\u65E5\u5FD7\u8BB0\u5F55\
  \u548C\u8C03\u8BD5\u76EE\u7684\u5C24\u5176\u6709\u7528\u3002\u7A0B\u5E8F\u5458\u8FD9\
  \u6837\u505A\u662F\u4E3A\u4E86\u5C06\u6B63\u5E38\u7A0B\u5E8F\u8F93\u51FA\u4E0E\u9519\
  \u8BEF\u6D88\u606F\u533A\u5206\u5F00\uFF0C\u4ECE\u800C\u5B9E\u73B0\u66F4\u6E05\u6670\
  \u7684\u8F93\u51FA\u7BA1\u7406\u548C\u66F4\u5BB9\u6613\u7684\u9519\u8BEF\u76D1\u63A7\
  \u3002"
title: "\u5199\u5165\u6807\u51C6\u9519\u8BEF"
---

{{< edit_this_page >}}

## 什么 & 为什么？
在JavaScript中写入标准错误（stderr）是指将错误消息或任何关键信息引导至一个特定的、独立的流中，这在Unix类环境中对于日志记录和调试目的尤其有用。程序员这样做是为了将正常程序输出与错误消息区分开，从而实现更清晰的输出管理和更容易的错误监控。

## 如何操作：
在Node.js中，可以使用`console.error()`方法或直接写入`process.stderr`来实现写入stderr。以下是展示这两种方法的示例：

```javascript
// 使用console.error()
console.error('这是一个错误消息。');

// 直接写入process.stderr
process.stderr.write('这是另一个错误消息。\n');
```

这两种方法的示例输出将会出现在stderr流中，不会与stdout混合：
```
这是一个错误消息。
这是另一个错误消息。
```

对于更复杂或特定于应用程序的日志记录，许多JavaScript程序员使用第三方库，如`winston`或`bunyan`。这里有一个使用`winston`的快速示例：

首先，通过npm安装`winston`：
```shell
npm install winston
```

然后，配置`winston`以将错误记录到stderr：
```javascript
const winston = require('winston');

const logger = winston.createLogger({
  levels: winston.config.syslog.levels,
  transports: [
    new winston.transports.Console({
      stderrLevels: ['error']
    })
  ]
});

// 记录一个错误消息
logger.error('通过winston记录的错误。');
```

这个设置确保当你使用`winston`记录错误时，它会将错误定向到stderr，帮助维护标准输出和错误输出之间的清晰分离。
