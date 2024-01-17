---
title:                "读取命令行参数"
html_title:           "Javascript: 读取命令行参数"
simple_title:         "读取命令行参数"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 什么是命令行参数？为什么程序员会使用它？

命令行参数是通过命令行（终端或命令提示符）向程序传递信息的一种方式。程序员会使用命令行参数来向程序提供输入，从而控制和定制程序的行为。它也可以让程序在每次运行时都接收不同的输入，方便程序员测试和调试代码。

## 如何实现：

```
// 命令行参数被保存在process.argv数组中，第一个元素是Node.js的路径，第二个元素是当前执行的脚本文件的路径
// 接下来的元素是命令行传递的参数
for (let i = 2; i < process.argv.length; i++) {
  console.log(process.argv[i]) // 输出每个参数
}
```

```
// 假设运行脚本时传递了两个参数：node script.js hello world
// output: hello
// output: world
```

## 深入了解：

1. 历史背景：在早期的计算机系统中，用户只能通过命令行来与计算机交互，因此命令行参数是必不可少的。随着图形化用户界面的发展，命令行的重要性有所下降，但命令行参数仍被广泛使用。

2. 其他方法：除了命令行参数，程序员还可以使用环境变量、配置文件等来传递输入。不同的方法具有不同的优缺点，程序员可以根据自己的需求选择最合适的方式。

3. 实现细节：更复杂的程序可能会使用第三方库来解析命令行参数，这可以让程序员更轻松地处理各种场景。同时，要注意处理用户输入时的异常情况，比如参数数量不符合预期。

## 相关资源：

- [Node.js官方文档 - 读取命令行参数](https://nodejs.org/api/process.html#process_process_argv)
- [CMDLine – JavaScript命令行解析模块](https://www.npmjs.com/package/cmdline)