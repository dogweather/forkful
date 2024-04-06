---
date: 2024-01-26 04:18:47.586896-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A TypeScript\u672C\u8EAB\u6CA1\u6709\u5185\
  \u7F6EREPL\u3002\u8BA9\u6211\u4EEC\u4F7F\u7528`ts-node`\uFF0C\u8FD9\u662F\u4E00\u4E2A\
  \u7528\u4E8ENode.js\u7684TypeScript\u6267\u884C\u73AF\u5883\uFF0C\u5305\u542B\u4E00\
  \u4E2AREPL\u3002 \u9996\u5148\uFF0C\u5168\u5C40\u5B89\u88C5\u5B83\uFF1A."
lastmod: '2024-04-05T22:38:46.631547-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A TypeScript\u672C\u8EAB\u6CA1\u6709\u5185\u7F6E\
  REPL\u3002\u8BA9\u6211\u4EEC\u4F7F\u7528`ts-node`\uFF0C\u8FD9\u662F\u4E00\u4E2A\u7528\
  \u4E8ENode.js\u7684TypeScript\u6267\u884C\u73AF\u5883\uFF0C\u5305\u542B\u4E00\u4E2A\
  REPL\u3002 \u9996\u5148\uFF0C\u5168\u5C40\u5B89\u88C5\u5B83\uFF1A."
title: "\u5728\u7F16\u7A0B\u4E2D\u4F7F\u7528\u4EA4\u4E92\u5F0FShell\uFF08REPL\uFF09"
weight: 34
---

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
