---
title:                "Javascript: 读取命令行参数"
simple_title:         "读取命令行参数"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# 为什么需要阅读命令行参数
在编写Javascript程序时，经常会遇到需要从命令行获取用户输入的情况。通过阅读命令行参数，我们可以轻松地获取用户输入并将其应用到我们的程序中，从而实现更加灵活和个性化的功能。

## 如何读取命令行参数
要读取命令行参数，我们需要使用process对象提供的属性和方法。首先，我们需要在程序中引入`process`模块，然后调用`process.argv`来获取用户输入的所有命令行参数。例如，假设我们要编写一个简单的程序来输出用户输入的所有参数，代码如下：

```Javascript
// 引入process模块
const process = require('process');

// 获取命令行参数
const args = process.argv;

// 遍历并输出所有参数
for (let i = 0; i < args.length; i++) {
    console.log(args[i]);
}
```

使用命令行运行我们的程序，输入以下命令：

```
node program.js arg1 arg2 arg3
```

则程序将会输出以下内容：

```
/Users/username/.nvm/versions/node/v10.16.0/bin/node // 我的本地路径可能不同
/Users/username/Desktop/program.js
arg1
arg2
arg3
```

我们可以看到，前两个参数是执行node程序的路径，接着才是我们输入的参数。这也是为什么我们在程序中需要用`for`循环来遍历参数的原因。

另外，我们可以通过指定`process.argv`的第三个参数来获取真正的命令行参数，即不包括执行node程序的路径和程序本身的路径。我们只需要修改上面的代码，如下所示：

```Javascript
// 获取命令行参数，从第三个参数开始
const args = process.argv.slice(2);
```

然后重新运行程序，结果就会变成：

```
arg1
arg2
arg3
```

## 深入了解命令行参数
除了通过`process.argv`来读取命令行参数外，我们还可以使用第三方模块`commander.js`来更加简单和灵活地处理参数。该模块不仅可以读取用户输入的参数，还可以自动生成帮助文档和对参数进行验证。

安装`commander.js`模块非常简单，只需在命令行运行以下命令即可：

```
npm install commander
```

然后，就可以在程序中引入该模块并开始使用了。具体使用方法，请参考官方文档。

## 查看更多
* [Node.js官方文档](https://nodejs.org/dist/latest-v12.x/docs/api/process.html#process_process_argv)
* [commander.js文档](https://github.com/tj/commander.js)