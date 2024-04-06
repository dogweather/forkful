---
date: 2024-01-20 17:56:21.650107-07:00
description: "How to: \u5982\u4F55\u64CD\u4F5C \u8981\u8BFB\u53D6\u547D\u4EE4\u884C\
  \u53C2\u6570\uFF0C\u53EF\u4EE5\u7528Node.js\u7684`process.argv`\u3002\u8FD9\u662F\
  \u4E2A\u5305\u542B\u6240\u6709\u547D\u4EE4\u884C\u53C2\u6570\u7684\u6570\u7EC4\u3002\
  \u7B2C\u4E00\u4E2A\u5143\u7D20\u662Fnode\uFF0C\u7B2C\u4E8C\u4E2A\u662F\u811A\u672C\
  \u6587\u4EF6\u540D\uFF0C\u5176\u4F59\u7684\u662F\u4F20\u5165\u7684\u53C2\u6570\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:48.508816-06:00'
model: gpt-4-1106-preview
summary: "\u5982\u4F55\u64CD\u4F5C \u8981\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570\
  \uFF0C\u53EF\u4EE5\u7528Node.js\u7684`process.argv`\u3002\u8FD9\u662F\u4E2A\u5305\
  \u542B\u6240\u6709\u547D\u4EE4\u884C\u53C2\u6570\u7684\u6570\u7EC4\u3002\u7B2C\u4E00\
  \u4E2A\u5143\u7D20\u662Fnode\uFF0C\u7B2C\u4E8C\u4E2A\u662F\u811A\u672C\u6587\u4EF6\
  \u540D\uFF0C\u5176\u4F59\u7684\u662F\u4F20\u5165\u7684\u53C2\u6570\u3002"
title: "\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570"
weight: 23
---

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
