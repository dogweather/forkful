---
date: 2024-01-26 04:15:29.613643-07:00
description: "\u4EA4\u4E92\u5F0Fshell\u6216\u8005REPL\uFF08\u8BFB\u53D6-\u8BC4\u4F30\
  -\u6253\u5370\u5FAA\u73AF\uFF09\uFF0C\u5141\u8BB8\u4F60\u5373\u65F6\u8FD0\u884C\u4EE3\
  \u7801\uFF0C\u6D4B\u8BD5\u51FD\u6570\u3001\u7B97\u6CD5\u6216\u8005\u628A\u73A9\u60F3\
  \u6CD5\u3002\u5B83\u4EEC\u662F\u7F16\u7801\u7684\u8349\u7A3F\u7EB8\uFF0C\u5FEB\u901F\
  \u4E14\u7B80\u964B\uFF0C\u65E0\u9700\u8BBE\u7F6E\u5B8C\u6574\u7684\u5F00\u53D1\u73AF\
  \u5883\u3002"
lastmod: '2024-03-11T00:14:22.019087-06:00'
model: gpt-4-0125-preview
summary: "\u4EA4\u4E92\u5F0Fshell\u6216\u8005REPL\uFF08\u8BFB\u53D6-\u8BC4\u4F30-\u6253\
  \u5370\u5FAA\u73AF\uFF09\uFF0C\u5141\u8BB8\u4F60\u5373\u65F6\u8FD0\u884C\u4EE3\u7801\
  \uFF0C\u6D4B\u8BD5\u51FD\u6570\u3001\u7B97\u6CD5\u6216\u8005\u628A\u73A9\u60F3\u6CD5\
  \u3002\u5B83\u4EEC\u662F\u7F16\u7801\u7684\u8349\u7A3F\u7EB8\uFF0C\u5FEB\u901F\u4E14\
  \u7B80\u964B\uFF0C\u65E0\u9700\u8BBE\u7F6E\u5B8C\u6574\u7684\u5F00\u53D1\u73AF\u5883\
  \u3002"
title: "\u5728\u7F16\u7A0B\u4E2D\u4F7F\u7528\u4EA4\u4E92\u5F0FShell\uFF08REPL\uFF09"
---

{{< edit_this_page >}}

## 什么 & 为什么？
交互式shell或者REPL（读取-评估-打印循环），允许你即时运行代码，测试函数、算法或者把玩想法。它们是编码的草稿纸，快速且简陋，无需设置完整的开发环境。

## 如何使用：
Node.js自带一个可通过终端访问的REPL。打开它，你就准备好了。来尝试一下：

```javascript
$ node
> let sum = (a, b) => a + b;
undefined
> sum(5, 10);
15
> .exit
```

直白，对吧？定义变量、函数或运行循环。完成后，`.exit`会带你回到现实世界。

## 深入探讨
自1960年代起，REPL就已存在 —— LISP开创了这一概念。这个想法是：给程序员即时反馈。还有其他选择吗？除了Node.js的REPL，还有基于浏览器的控制台，如Chrome DevTools，在线沙箱，如JSFiddle，或完整的IDE，如VSCode，带有交互式游乐场。

在底层，REPL工作流通常包括：
1. 读取输入
2. 编译并执行代码
3. 打印输出
4. 回到循环

这是一个简单却有效的循环，对交互式编码产生了巨大影响。

## 另见
- [Node.js REPL文档](https://nodejs.org/api/repl.html)
- [Mozilla关于REPL中JavaScript模块的介绍](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Modules)
- [JSFiddle](https://jsfiddle.net/)
