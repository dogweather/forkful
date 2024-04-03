---
date: 2024-01-26 03:48:27.123226-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Clojure\u4F9D\u8D56Java\u865A\u62DF\u673A\
  \uFF08JVM\uFF09\uFF0C\u56E0\u6B64\u5F88\u591A\u8C03\u8BD5\u5DE5\u4F5C\u90FD\u662F\
  \u7528Java\u5DE5\u5177\u5B8C\u6210\u7684\u3002\u5176\u4E2D\u4E00\u4E2A\u5DE5\u5177\
  \u662F`CIDER`\uFF0C\u8FD9\u662F\u4E00\u4E2A\u4E3AClojure\u5F00\u53D1\u5728Emacs\u4E2D\
  \u63D0\u4F9B\u7684\u5F3A\u5927\u8F6F\u4EF6\u5305\uFF0C\u5177\u6709\u53EF\u9760\u7684\
  \u8C03\u8BD5\u80FD\u529B\u3002\u6211\u4EEC\u6765\u6DF1\u5165\u4E86\u89E3\u4E00\u4E0B\
  \uFF1A."
lastmod: '2024-03-13T22:44:47.307606-06:00'
model: gpt-4-0125-preview
summary: "Clojure\u4F9D\u8D56Java\u865A\u62DF\u673A\uFF08JVM\uFF09\uFF0C\u56E0\u6B64\
  \u5F88\u591A\u8C03\u8BD5\u5DE5\u4F5C\u90FD\u662F\u7528Java\u5DE5\u5177\u5B8C\u6210\
  \u7684\u3002\u5176\u4E2D\u4E00\u4E2A\u5DE5\u5177\u662F`CIDER`\uFF0C\u8FD9\u662F\u4E00\
  \u4E2A\u4E3AClojure\u5F00\u53D1\u5728Emacs\u4E2D\u63D0\u4F9B\u7684\u5F3A\u5927\u8F6F\
  \u4EF6\u5305\uFF0C\u5177\u6709\u53EF\u9760\u7684\u8C03\u8BD5\u80FD\u529B\u3002\u6211\
  \u4EEC\u6765\u6DF1\u5165\u4E86\u89E3\u4E00\u4E0B\uFF1A."
title: "\u4F7F\u7528\u8C03\u8BD5\u5668"
weight: 35
---

## 如何操作：
Clojure依赖Java虚拟机（JVM），因此很多调试工作都是用Java工具完成的。其中一个工具是`CIDER`，这是一个为Clojure开发在Emacs中提供的强大软件包，具有可靠的调试能力。我们来深入了解一下：

```clojure
;; 首先，在Emacs中使用CIDER接入一个Clojure项目
M-x cider-jack-in

;; 设置一个断点
;; 导航到你想要检查的Clojure代码行并
;; 按"C-c M-b" 或执行：
M-x cider-debug-defun-at-point

;; 当代码运行时，你会遇到断点。CIDER会提示你：
;; 1. n 进入执行的下一逻辑步骤，
;; 2. c 继续执行直到下一个断点，
;; 3. q 退出调试。

;; 在断点处检查局部变量
;; 在断点处，输入：
locals

;; 你会在minibuffer中看到一个局部变量及其值的列表被打印出来。
```
样例输出可能看起来像这样：
```clojure
{:x 10, :y 20, :result 200}
```

## 深入探讨
调试器是计算领域中古老的工具。“bug”这个词是在计算机早期，一个实际的昆虫通过短路一个机器中的电路导致错误时被创造出来的。

虽然`CIDER`对于Emacs爱好者来说非常棒，但还有其他Clojure调试的替代方案。例如，使用IntelliJ和Cursive插件可以提供一个更基于GUI的调试体验。此外，您还可以利用内置的Leiningen或tools.deps来控制调试过程的流程。

从底层来看，这些调试器常常操纵字节码，在专门的nREPL会话中执行评估，并提供堆栈跟踪检查。它们利用了底层JVM的能力，挖掘了Java调试框架的财富。

## 另见
- [CIDER调试器文档](https://docs.cider.mx/cider/debugging/debugger.html)
- [Cursive调试器](https://cursive-ide.com/userguide/debugging.html)
- [Leiningen用于自动化和调试](https://leiningen.org/)
- [tools.deps.alpha用于更多控制](https://github.com/clojure/tools.deps.alpha)
