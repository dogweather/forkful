---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:43.615065-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728Node.js\u4E2D\uFF0C\u53EF\u4EE5\
  \u4F7F\u7528`console.error()`\u65B9\u6CD5\u6216\u76F4\u63A5\u5199\u5165`process.stderr`\u6765\
  \u5B9E\u73B0\u5199\u5165stderr\u3002\u4EE5\u4E0B\u662F\u5C55\u793A\u8FD9\u4E24\u79CD\
  \u65B9\u6CD5\u7684\u793A\u4F8B\uFF1A."
lastmod: '2024-04-05T21:53:48.509982-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u5199\u5165\u6807\u51C6\u9519\u8BEF"
weight: 25
---

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
