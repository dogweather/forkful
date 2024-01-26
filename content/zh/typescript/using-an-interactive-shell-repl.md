---
title:                "在编程中使用交互式Shell（REPL）"
date:                  2024-01-26T04:18:47.586896-07:00
model:                 gpt-4-0125-preview
simple_title:         "在编程中使用交互式Shell（REPL）"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
读取-执行-打印-循环（REPL）是一种编程环境，它接受单个用户输入，执行它们，并将结果返回给用户。程序员使用REPL来快速试验代码片段、调试以及学习新语言特性，而无需创建完整的应用程序的开销。

## 如何操作：
TypeScript本身没有内置REPL。让我们使用`ts-node`，这是一个用于Node.js的TypeScript执行环境，包含一个REPL。

首先，全局安装它：
```bash
npm install -g ts-node
```

通过在命令行中键入`ts-node`来启动REPL：
```bash
ts-node
```

这里有一个要试验的快速代码片段：
```TypeScript
> let message: string = '你好，REPL！';
> console.log(message);
你好，REPL！
> 
```
要结束会话，请按`Ctrl+D`。

## 深入了解
从历史上看，REPLs在像Lisp这样的语言中非常突出，允许动态代码评估。这个概念自那以后已经传播开来，成为许多语言中交互式编程的一个基本工具。

对于TypeScript来说，`ts-node`并不是你唯一的选择。替代方案包括使用Web浏览器中的TypeScript Playground或利用其他支持TypeScript的Node.js基础的REPL，并使用适当的插件。

就实现而言，`ts-node`使用TypeScript编译器API来即时转译代码，然后由Node.js执行。这为你提供即时反馈，尤其适用于尝试TypeScript的最新功能，无需设置麻烦。

需要记住的一件事 - 虽然REPL非常适合快速测试，但它并不能取代编写传统的、可测试的和可维护的代码。它是一个用于学习和探索的工具，不是适当开发实践的替代品。

## 另请参阅
- [TypeScript官方网站](https://www.typescriptlang.org/)
- [ts-node在GitHub上](https://github.com/TypeStrong/ts-node)
- [Node.js REPL文档](https://nodejs.org/api/repl.html)
- [TypeScript Playground](https://www.typescriptlang.org/play)