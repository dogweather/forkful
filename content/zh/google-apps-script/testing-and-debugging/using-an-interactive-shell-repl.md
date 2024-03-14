---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:03:57.465242-07:00
description: "\u4EA4\u4E92\u5F0Fshell\uFF0C\u6216\u8005\u8BF4\u8BFB\u53D6-\u6C42\u503C\
  -\u6253\u5370\u5FAA\u73AF\uFF08REPL\uFF09\uFF0C\u662F\u4E00\u4E2A\u7B80\u5355\u7684\
  \u4EA4\u4E91\u7F16\u7A0B\u73AF\u5883\uFF0C\u5B83\u63A5\u6536\u7528\u6237\u7684\u5355\
  \u4E2A\u8F93\u5165\uFF08\u8868\u8FBE\u5F0F\uFF09\uFF0C\u5BF9\u5176\u8FDB\u884C\u6C42\
  \u503C\uFF0C\u5E76\u5C06\u7ED3\u679C\u8FD4\u56DE\u7ED9\u7528\u6237\u3002\u7A0B\u5E8F\
  \u5458\u4F7F\u7528REPL\u8FDB\u884C\u5FEB\u901F\u539F\u578B\u8BBE\u8BA1\u3001\u8C03\
  \u8BD5\u4EE5\u53CA\u4EE5\u4EA4\u4E92\u65B9\u5F0F\u5B66\u4E60\u7F16\u7A0B\u8BED\u8A00\
  \u7684\u8BED\u6CD5\u548C\u884C\u4E3A\u3002"
lastmod: '2024-03-13T22:44:47.203688-06:00'
model: gpt-4-0125-preview
summary: "\u4EA4\u4E92\u5F0Fshell\uFF0C\u6216\u8005\u8BF4\u8BFB\u53D6-\u6C42\u503C\
  -\u6253\u5370\u5FAA\u73AF\uFF08REPL\uFF09\uFF0C\u662F\u4E00\u4E2A\u7B80\u5355\u7684\
  \u4EA4\u4E91\u7F16\u7A0B\u73AF\u5883\uFF0C\u5B83\u63A5\u6536\u7528\u6237\u7684\u5355\
  \u4E2A\u8F93\u5165\uFF08\u8868\u8FBE\u5F0F\uFF09\uFF0C\u5BF9\u5176\u8FDB\u884C\u6C42\
  \u503C\uFF0C\u5E76\u5C06\u7ED3\u679C\u8FD4\u56DE\u7ED9\u7528\u6237\u3002\u7A0B\u5E8F\
  \u5458\u4F7F\u7528REPL\u8FDB\u884C\u5FEB\u901F\u539F\u578B\u8BBE\u8BA1\u3001\u8C03\
  \u8BD5\u4EE5\u53CA\u4EE5\u4EA4\u4E92\u65B9\u5F0F\u5B66\u4E60\u7F16\u7A0B\u8BED\u8A00\
  \u7684\u8BED\u6CD5\u548C\u884C\u4E3A\u3002"
title: "\u4F7F\u7528\u4EA4\u4E92\u5F0FShell\uFF08REPL\uFF09"
---

{{< edit_this_page >}}

## 什么 & 为什么？

交互式shell，或者说读取-求值-打印循环（REPL），是一个简单的交云编程环境，它接收用户的单个输入（表达式），对其进行求值，并将结果返回给用户。程序员使用REPL进行快速原型设计、调试以及以交互方式学习编程语言的语法和行为。

## 如何操作：

Google Apps Script，一个基于云的脚本语言，用于跨谷歌产品自动化任务，没有类似于Python或JavaScript的Node.js那样的内置REPL工具。然而，你可以通过使用Apps Script编辑器的日志和调试功能，或者设置一个外部环境来模拟类似的体验。这里，我们关注在Apps Script编辑器内创建一个临时的REPL。

1. **创建一个临时REPL函数**：

```javascript
function myREPL() {
  var input = Logger.log('请输入你的表达式: ');
  try {
    var result = eval(input);
    Logger.log('结果: ' + result);
  } catch(e) {
    Logger.log('错误: ' + e.message);
  }
}
```

由于在Apps Script环境中不可能以传统REPL相同的方式直接进行用户输入，你可以手动修改`input`变量并运行`myREPL()`来测试表达式。

2. **示例代码执行**：

比方说你希望计算`2+2`。你应该如下修改`myREPL`函数：

```javascript
function myREPL() {
  var input = '2+2'; // 在此手动输入你的表达式
  // 其余保持不变...
}
```

运行`myREPL()`后，检查日志（查看 > 日志）以获取输出，输出内容应类似于：

```
[20-xx-xxxx xx:xx:xx:xxx] 请输入你的表达式:
[20-xx-xxxx xx:xx:xx:xxx] 结果: 4
```

3. **使用Logger调试**：

对于更复杂的调试，可在代码中插入`Logger.log(variable);`以打印变量状态，帮助你理解脚本的流程和中间状态。

## 深入了解

REPL的概念在计算历史中根深蒂固，源自1960年代的分时系统，它允许进行交互式会话。像Lisp这样的语言在这种环境中蓬勃发展，因为REPL对于它们的迭代开发过程至关重要。相比之下，Google Apps Script在很晚之后出现，主要面向网络，专注于在谷歌套件中自动化任务，而不是迭代的、基于控制台的编程。

由于其基于云和专注于Web应用部署的性质，Google Apps Script传统上不支持开箱即用的实时、交互式编码会话。其执行模型围绕通过Web事件、时间驱动的触发器或在环境内手动调用的函数旋转，而非REPL提供的即时反馈循环。

虽然Apps Script编辑器内的临时REPL和调试器提供了一定程度的交互性，但它们并没有完全复制许多编程语言中传统REPL所提供的即时反馈和效率。寻求与谷歌技术更真实REPL体验的开发者可能会探索外部的JavaScript环境或Node.js与谷歌的APIs。这些可以提供更响应式和交互式的编码会话，尽管可能需要更多的设置，并且可能需要走出直接的Apps Script环境。
