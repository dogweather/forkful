---
title:                "读取命令行参数"
html_title:           "C: 读取命令行参数"
simple_title:         "读取命令行参数"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？

命令行参数是在命令行运行程序时传送给程序的输入。程序员用它来控制程序，影响运行结果。

## 怎么做：

在Javascript中，我们可以用`process.argv`数组来读取命令行参数。例如：

```Javascript
// myscript.js
console.log(process.argv);
```
运行命令行`node myscript.js Hello World`，将得到以下输出：

```Javascript
['/usr/local/bin/node',
 '/path/to/myscript.js',
 'Hello',
 'World']
```

第一个元素是node路径，第二个元素是当前脚本路径。后来的元素是传入的参数。

## 深入：

读取命令行参数在Unix和Linux Shell脚本中非常常见。现在我们可以在Node.js中使用此功能，这使得JavaScript的用途更加广泛。

当前，也有一些库（例如 minimist ）提供更高级更方便的方式处理命令行参数。

在内部，`process.argv`读取来自Node.js内置的`process`模块的数据，这个模块提供了有关当前Node.js程序实例的信息。

## 参考：

[Node.js process.argv文档](https://nodejs.org/docs/latest/api/process.html#process_process_argv) 

[minimist库](https://github.com/substack/minimist)

[关于Unix/Linux Shell脚本传参的介绍](https://www.tutorialspoint.com/unix/unix-using-variables.htm)