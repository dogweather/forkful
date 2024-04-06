---
date: 2024-01-26 04:15:29.613643-07:00
description: "\u5982\u4F55\u4F7F\u7528\uFF1A Node.js\u81EA\u5E26\u4E00\u4E2A\u53EF\
  \u901A\u8FC7\u7EC8\u7AEF\u8BBF\u95EE\u7684REPL\u3002\u6253\u5F00\u5B83\uFF0C\u4F60\
  \u5C31\u51C6\u5907\u597D\u4E86\u3002\u6765\u5C1D\u8BD5\u4E00\u4E0B\uFF1A."
lastmod: '2024-04-05T21:53:48.494530-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u5728\u7F16\u7A0B\u4E2D\u4F7F\u7528\u4EA4\u4E92\u5F0FShell\uFF08REPL\uFF09"
weight: 34
---

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
