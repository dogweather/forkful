---
title:                "在编程中使用交互式Shell（REPL）"
aliases:
- zh/javascript/using-an-interactive-shell-repl.md
date:                  2024-01-26T04:15:29.613643-07:00
model:                 gpt-4-0125-preview
simple_title:         "在编程中使用交互式Shell（REPL）"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/using-an-interactive-shell-repl.md"
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
