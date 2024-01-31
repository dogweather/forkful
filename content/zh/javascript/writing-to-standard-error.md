---
title:                "写入标准错误"
date:                  2024-01-19
html_title:           "Arduino: 写入标准错误"
simple_title:         "写入标准错误"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? 什麼和為什麼？
写入标准错误（stderr）用于输出错误信息和日志。程序员这么做是为了分离正常输出和错误信息，便于调试和日志记录。

## How to: 如何操作：
在JavaScript中，你可以使用`console.error()`写入标准错误：

```Javascript
console.error("这是一个错误信息。");
```

样例输出：

```
这是一个错误信息。
```

你也可以使用`process.stderr.write()`在Node.js里直接写入标准错误：

```Javascript
process.stderr.write("直接写入标准错误。\n");
```

样例输出：

```
直接写入标准错误。
```

## Deep Dive 深入探究
历史上，stderr是UNIX中的标准流概念之一，用于帮助区分正常输出和错误信息。虽然`console.error()`和`process.stderr.write()`都可以用来写入错误信息，但`console.error()`在输出前会先格式化信息，而`process.stderr.write()`则可以直接写入，没有格式化。Node.js遵循这一标准，提供了这些方法来帮助开发者管理输出。

## See Also 参考链接
- Node.js官方文档关于`process`对象: [Node.js Process](https://nodejs.org/api/process.html#process_process_stderr)
- MDN Web Docs 关于`console.error()`的说明: [console.error()](https://developer.mozilla.org/en-US/docs/Web/API/Console/error)
