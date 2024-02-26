---
date: 2024-01-26 04:18:47.586896-07:00
description: "\u8BFB\u53D6-\u6267\u884C-\u6253\u5370-\u5FAA\u73AF\uFF08REPL\uFF09\u662F\
  \u4E00\u79CD\u7F16\u7A0B\u73AF\u5883\uFF0C\u5B83\u63A5\u53D7\u5355\u4E2A\u7528\u6237\
  \u8F93\u5165\uFF0C\u6267\u884C\u5B83\u4EEC\uFF0C\u5E76\u5C06\u7ED3\u679C\u8FD4\u56DE\
  \u7ED9\u7528\u6237\u3002\u7A0B\u5E8F\u5458\u4F7F\u7528REPL\u6765\u5FEB\u901F\u8BD5\
  \u9A8C\u4EE3\u7801\u7247\u6BB5\u3001\u8C03\u8BD5\u4EE5\u53CA\u5B66\u4E60\u65B0\u8BED\
  \u8A00\u7279\u6027\uFF0C\u800C\u65E0\u9700\u521B\u5EFA\u5B8C\u6574\u7684\u5E94\u7528\
  \u7A0B\u5E8F\u7684\u5F00\u9500\u3002"
lastmod: '2024-02-25T18:49:45.036591-07:00'
model: gpt-4-0125-preview
summary: "\u8BFB\u53D6-\u6267\u884C-\u6253\u5370-\u5FAA\u73AF\uFF08REPL\uFF09\u662F\
  \u4E00\u79CD\u7F16\u7A0B\u73AF\u5883\uFF0C\u5B83\u63A5\u53D7\u5355\u4E2A\u7528\u6237\
  \u8F93\u5165\uFF0C\u6267\u884C\u5B83\u4EEC\uFF0C\u5E76\u5C06\u7ED3\u679C\u8FD4\u56DE\
  \u7ED9\u7528\u6237\u3002\u7A0B\u5E8F\u5458\u4F7F\u7528REPL\u6765\u5FEB\u901F\u8BD5\
  \u9A8C\u4EE3\u7801\u7247\u6BB5\u3001\u8C03\u8BD5\u4EE5\u53CA\u5B66\u4E60\u65B0\u8BED\
  \u8A00\u7279\u6027\uFF0C\u800C\u65E0\u9700\u521B\u5EFA\u5B8C\u6574\u7684\u5E94\u7528\
  \u7A0B\u5E8F\u7684\u5F00\u9500\u3002"
title: "\u5728\u7F16\u7A0B\u4E2D\u4F7F\u7528\u4EA4\u4E92\u5F0FShell\uFF08REPL\uFF09"
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
