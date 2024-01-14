---
title:    "TypeScript: 读取命令行参数"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 为什么要读取命令行参数？

读取命令行参数是一项非常有用的技能，特别是对于那些经常需要从命令行操作的开发人员来说。通过读取命令行参数，您可以在程序运行时向程序传递不同的参数，从而改变程序的行为。这样可以提高程序的灵活性和可定制性，使得程序可以更好地满足不同的需求。

## 如何读取命令行参数？

为了读取命令行参数，我们可以使用process对象提供的argv属性。这个属性返回一个字符串数组，其中包含了在命令行中输入的所有参数。下面是一个示例代码，展示了如何读取命令行参数并打印出来：

```TypeScript
const args = process.argv;
args.forEach(arg => console.log(arg));
```

假设我们在命令行中输入了以下命令： `node index.ts arg1 arg2 arg3`，那么上面的例子代码将会输出以下内容：

```
node
index.ts
arg1
arg2
arg3
```

我们可以通过使用数组的`slice()`方法来获取我们想要的参数，比如从第三个参数开始。下面是一个更复杂的示例，展示了如何读取命令行参数并把它们转换成数字：

```TypeScript
const args = process.argv.slice(2);
const numArgs = args.map(arg => parseInt(arg, 10));
console.log(numArgs);
```

如果我们输入了命令 `node index.ts 5 10 15`，那么上面的代码将会输出以下内容：

```
[5, 10, 15]
```

## 深入了解读取命令行参数

除了使用argv属性外，还可以使用第三方库来处理命令行参数，比如yargs库。这个库提供了更多的功能，比如解析命令行参数并提供帮助文档和错误处理。如果您需要更复杂的命令行参数处理，可以考虑使用这个库。

此外，还可以通过使用TS命令行工具来简化读取命令行参数的过程。这个工具可以自动生成带有类型检查的命令行参数解析器，减少开发人员的工作量。

## 参考链接

- [TypeScript命令行工具](https://github.com/TypeStrong/ts-command-line)
- [yargs库](https://github.com/yargs/yargs)
- [process.argv文档](https://nodejs.org/api/process.html#process_process_argv)