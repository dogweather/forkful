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

为什么： 为什么要阅读命令行参数 - 最多2句话
--------------------------------------
命令行参数是一种非常有用的技术，它允许我们在运行程序时传递一些额外的信息。这样，我们可以在不改变程序本身的情况下，根据不同的需求来调整程序的行为。 如果你想要探索这个技术，就让我们一起来看看吧！

### 如何实现

在Javascript中，我们可以使用process对象来读取命令行参数。我们可以通过process.argv来获取所有的参数，它是一个字符串数组。第一个参数是node的路径，第二个参数是我们执行的文件的路径。之后的元素都是我们传递的参数。

```Javascript
// 获取所有命令行参数
const args = process.argv;

// 获取特定位置的参数
const paramOne = args[2];
const paramTwo = args[3];

console.log(paramOne, paramTwo);
```

假设我们执行 `node app.js hello world`， 那么控制台会打印出 `hello world`。如果我们传递更多的参数，它们也会被打印出来。

### 深入了解

除了使用process.argv来读取命令行参数，还有一些工具可以帮助我们处理更复杂的情况。比如，第三方库yargs可以帮助我们解析命令行参数并将其转换为对象，方便我们使用。

```Javascript
// 使用yargs来解析命令行参数
const argv = require('yargs').argv;

// 使用 -- 前缀来传递参数
// 如 --name 和 --age
console.log(argv.name, argv.age);
```

但要注意，这些工具都是基于process.argv来实现的。因此，了解如何在原生的Javascript中读取命令行参数还是很重要的。

### 查看更多

如果你想要深入学习命令行参数的使用，可以查看以下文档和教程：

- [Node.js process.argv 文档](https://nodejs.org/api/process.html#process_process_argv)
- [yargs 官方文档](https://www.npmjs.com/package/yargs)
- [The Net Ninja 的命令行参数教程 (视频)](https://www.youtube.com/watch?v=XdIWmGFAW5w)

### 参考链接：

- [Node.js process.argv documentation](https://nodejs.org/api/process.html#process_process_argv)
- [Official yargs documentation](https://www.npmjs.com/package/yargs)
- [The Net Ninja's command line arguments tutorial (video)](https://www.youtube.com/watch?v=XdIWmGFAW5w)