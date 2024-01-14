---
title:                "Javascript: 读取命令行参数"
programming_language: "Javascript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# 为什么阅读命令行参数

阅读命令行参数是一种非常重要的技能，它可以帮助程序员更高效地运行和调试他们的代码。通过学习如何读取命令行参数，你可以轻松地获取用户输入的信息，从而使你的程序更加稳定和交互性。

# 如何读取命令行参数

使用JavaScript读取命令行参数非常简单。首先，在你的代码中引入内置的“process”模块。然后，使用“process.argv”方法来获取所有的命令行参数，它返回一个数组，其中第一个元素是你的文件路径，后面的元素是用户输入的参数。下面是一个例子：

```javascript
const process = require('process');
console.log(process.argv);
```
假设你在命令行运行“node index.js apple banana”，那么上面的代码输出将会是：["/path/to/index.js", "apple", "banana"]。这样，你就可以轻松地读取用户输入的参数，并在程序中使用它们了。

# 深入了解命令行参数

除了上面提到的“process.argv”方法外，你还可以使用“yargs”这个包来更加灵活地读取和解析命令行参数。它提供了很多有用的方法来帮助你处理用户输入，比如可以设置只接受特定类型的参数、设置别名和默认值等。下面是一个使用“yargs”包的例子：

```javascript
const yargs = require('yargs');
const argv = yargs
  .option('name', {
    alias: 'n',
    description: 'Your name',  
    type: 'string',
  })
  .option('age', {
    alias: 'a',
    description: 'Your age',
    type: 'number',
  })
  .help() 
  .alias('help', 'h') 
  .argv;
console.log(`Your name is ${argv.name} and your age is ${argv.age}.`);
```

假设你在命令行运行“node index.js --name Jane --age 20”，那么你将会看到输出：Your name is Jane and your age is 20。你也可以通过“--help”来查看可用的选项和参数。

# 浏览更多

如果你想深入了解命令行参数的知识，可以参考下面的资源：

- [Node.js文档中关于命令行参数的说明](https://nodejs.org/docs/latest/api/process.html#process_process_argv)
- [Yargs官方文档](https://yargs.js.org/)
- [阮一峰老师的命令行参数教程](http://javascript.ruanyifeng.com/nodejs/module.html#toc5)
- [命令行参数示例代码](https://github.com/mandareebs/mandareebs.github.io/tree/master/examples/javascript/command-line-arguments)

# 还有其他需要了解的内容吗？

去看看下面的链接吧！

- [Markdown官方文档](https://daringfireball.net/projects/markdown/)
- [如何使用Markdown编写博客文章](https://www.zhihu.com/question/23314188)