---
title:                "写入标准错误"
date:                  2024-02-03T19:33:43.615065-07:00
model:                 gpt-4-0125-preview
simple_title:         "写入标准错误"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
