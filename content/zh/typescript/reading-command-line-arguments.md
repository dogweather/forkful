---
title:                "TypeScript: 读取命令行参数"
programming_language: "TypeScript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 为什么

读取命令行参数是一个在编程中非常有用的技巧。通过读取命令行参数，你可以使你的程序更加灵活和可配置。例如，你可以允许用户在运行程序时指定不同的参数，从而改变程序的行为。

## 如何

在TypeScript中，你可以使用`process.argv`来读取命令行参数。下面是一个简单的示例：

```TypeScript
console.log("你输入的参数为：", process.argv[2]);
```

当你运行这段代码时，如果在命令行中输入了一个参数（例如`node index.js hello`），你会看到以下输出：

```
你输入的参数为：hello
```

你可以通过`process.argv`数组来访问所有的命令行参数。第一个元素是Node.js的路径，第二个元素是被执行的文件路径，之后的每个元素都是一个命令行参数。

## 深入了解

除了直接读取`process.argv`之外，还有一些其他的方法来更好地处理命令行参数，例如使用`commander`库。这个库可以帮助你解析命令行参数，并提供更多的功能，例如自动生成帮助文档。如果你想进一步了解如何使用`commander`库，可以查看以下资源：

- [官方文档](https://github.com/tj/commander.js#readme)
- [实例教程](https://www.sitepoint.com/javascript-command-line-interface-cli-node-js/)

## 参考链接

- [Node.js文档 - Process对象](https://nodejs.org/api/process.html#process_process_argv)
- [Node.js案例教程 - 传递命令行参数](https://www.guru99.com/node-js-command-line-arguments.html)
- [TypeScript入门教程 - 访问命令行参数](https://www.typescriptlang.org/docs/handbook/intro.html#command-line-arguments)
- [马克飞象编辑器（支持Markdown语法）](https://maxiang.io/)