---
title:                "读取命令行参数"
aliases:
- /zh/javascript/reading-command-line-arguments.md
date:                  2024-01-20T17:56:21.650107-07:00
model:                 gpt-4-1106-preview
simple_title:         "读取命令行参数"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? 什么和为什么？
JavaScript中读取命令行参数，就是获取在终端运行脚本时传入的额外信息。程序员这么做是为了让程序更灵活，可根据不同的参数执行不同的任务。

## How to: 如何操作
要读取命令行参数，可以用Node.js的`process.argv`。这是个包含所有命令行参数的数组。第一个元素是node，第二个是脚本文件名，其余的是传入的参数。

```Javascript
// save as greetings.js

// 这段代码显示如何获取命令行参数
const args = process.argv.slice(2); // 去除数组中的前两个元素

console.log(`Hello, ${args[0]}!`);

// 运行: node greetings.js World
// 输出: Hello, World!
```

## Deep Dive 深入研究
历史上，读取命令行参数是C语言和其他低级编程语言常见的功能。Node.js通过`process.argv`让JavaScript也拥有了这个能力。作为替代，一些库如`yargs`或`commander`可提供更多功能和更好的解析选项。

实现细节方面，`process.argv`会解析任何传递给Node.js脚本的参数，但处理复杂命令行参数时，可能需要自定义解析逻辑或使用第三方库。

## See Also 另请参阅

- Node.js官方文档关于`process.argv`的部分：[https://nodejs.org/docs/latest/api/process.html#process_process_argv](https://nodejs.org/docs/latest/api/process.html#process_process_argv)
- `yargs`库文档：[https://yargs.js.org/](https://yargs.js.org/)
- `commander`库文档：[https://www.npmjs.com/package/commander](https://www.npmjs.com/package/commander)
