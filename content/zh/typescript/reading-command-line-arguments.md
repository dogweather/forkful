---
title:                "读取命令行参数"
html_title:           "TypeScript: 读取命令行参数"
simple_title:         "读取命令行参数"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 为什么

读取命令行参数是在编写命令行工具和脚本时非常常见的操作。通过阅读本文，您将了解如何在TypeScript中读取命令行参数，并可以将这一技巧应用到自己的项目中。

## 如何操作

读取命令行参数可以通过使用Node.js内置的process对象来实现。首先，我们需要在项目中安装@types/node，这样就可以在TypeScript中使用Node.js的类型声明。

```TypeScript
npm install --save-dev @types/node
```

接下来，在您的TypeScript文件中，使用import命令引入process对象。

```TypeScript
import process from 'process';
```

现在，我们可以使用process对象的argv属性来读取命令行参数。这个属性将包含所有传递给命令行的参数，包括脚本路径及其他参数。

```TypeScript
const args = process.argv;

// 打印出所有命令行参数
console.log(args);
```

运行这段代码，您将看到类似下面这样的输出：

```
['/usr/local/bin/node', 'index.ts', 'arg1', 'arg2', 'arg3']
```

注意，第一个参数是Node.js执行的脚本路径，之后的参数才是我们实际传递的命令行参数。

如果您只想要获取我们传递的实际参数，可以使用slice方法来剔除前两个参数：

```TypeScript
const args = process.argv.slice(2);

// 打印出所有实际传递的参数
console.log(args);
```

现在的输出将只包含我们传递的实际参数：

```
['arg1', 'arg2', 'arg3']
```

如果您想要以键值对的形式读取命令行参数，您可以使用一个循环来将每个参数拆分成key和value，然后存储到一个对象中：

```TypeScript
const args = process.argv.slice(2);
const options = {};

// 循环读取所有参数并拆分成key和value存入options对象中
for (let i = 0; i < args.length; i++) {
  const key = args[i].split('=')[0];
  const value = args[i].split('=')[1];
  options[key] = value;
}

// 打印出所有参数键值对
console.log(options);
```

如果我们传递的参数为：`--name=John Doe --age=30 --city=New York`，将会得到以下输出：

```
{ name: 'John Doe', age: '30', city: 'New York' }
```

## 深入了解

除了使用Node.js的process对象，您还可以使用第三方库来更轻松地读取命令行参数，比如yargs和commander。这些库提供了更强大的功能，比如解析参数选项、设置默认值等。如果您需要在您的项目中使用更高级的命令行参数处理，可以考虑使用这些库。

## 参考链接

- [Node.js process文档](https://nodejs.org/api/process.html)
- [yargs GitHub仓库](https://github.com/yargs/yargs)
- [commander GitHub仓库](https://github.com/tj/commander.js)