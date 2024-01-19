---
title:                "读取命令行参数"
html_title:           "C: 读取命令行参数"
simple_title:         "读取命令行参数"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 到底是什么以及为什么?

命令行参数是在启动程序时传递给主函数的参数。程序员通过读取这些参数，使程序更加灵活，可以对输入和行为进行自定义。

## 如何操作:

我们可以通过process.argv来读取命令行参数。以下是一些示例:

```typescript 
var myArgs = process.argv.slice(2);
console.log('myArgs: ', myArgs);
```
在命令行中运行 `node file_name.js one two=three four` 会返回:

```
myArgs:  [ 'one', 'two=three', 'four' ]
```

## 深入解析:

历史上，从命令行读取参数的能力是编程的基础，这几乎与编程本身一样古老。一些替代方案像commander.js、yargs等能够使处理命令行参数更简单。

实现细节上，在Node.js环境下，process.argv数组包含了命令行参数。数组的第一个元素为 'node'，第二个元素为正在被执行的文件的路径，后面的元素为附加的命令行参数。

##参阅:

这些是一些有用的资源

1. Node.js的process.argv文档: [https://nodejs.org/docs/latest/api/process.html#process_process_argv](https://nodejs.org/docs/latest/api/process.html#process_process_argv)
2. Commander.js: [https://www.npmjs.com/package/commander](https://www.npmjs.com/package/commander)
3. Yargs: [https://www.npmjs.com/package/yargs](https://www.npmjs.com/package/yargs)