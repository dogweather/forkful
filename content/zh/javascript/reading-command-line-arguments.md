---
title:    "Javascript: 读取命令行参数"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 为什么要读取命令行参数？

命令行参数是指在运行Javascript程序时，用户可以在命令行中输入的参数。它们可以帮助我们在运行程序时提供额外的信息，从而改变程序的行为。因此，学习如何读取命令行参数可以帮助我们更好地控制我们的程序。

## 如何读取命令行参数？

在Javascript中，我们可以使用process对象来读取命令行参数。process对象是一个全局对象，它提供了与当前进程有关的信息，包括命令行参数。

下面是一个简单的代码示例，演示如何在Javascript中读取命令行参数：

```Javascript
// 假设我们的程序名为"sayHello.js"
// 在命令行中运行：node sayHello.js John
// 则"John"将作为参数传递给程序
const name = process.argv[2]; // 命令行参数以数组的形式保存在process.argv中
console.log(`Hello ${name}!`); // 输出：Hello John!
```

通过这种方式，我们可以在运行程序时，通过在命令行输入不同的参数来改变程序的输出。

## 深入了解读取命令行参数

除了通过process对象来读取命令行参数之外，我们还可以使用第三方库来帮助我们更方便地读取命令行参数。例如，"yargs"这个库可以帮助我们解析命令行参数，并提供更多的功能，如设置选项和命令别名等。

另外，需要注意的是，在读取命令行参数时，我们也可以通过命令行选项来指定参数的类型。例如，使用"--n 10"来表示参数n的值为整数10。这样就可以帮助我们更准确地读取和使用命令行参数。

## 参考链接

- [Node.js官方文档 - process](https://nodejs.org/api/process.html)
- [yargs官方文档](http://yargs.js.org/)
- [阮一峰的《Node.js教程 - process对象》](https://www.ruanyifeng.com/blog/2015/05/process.html)

## 参见

- [Markdown语法指南](https://www.markdownguide.org/)
- [Javascript命令行工具开发教程](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Building_blocks/CommandLine#Reading_command_line_arguments)