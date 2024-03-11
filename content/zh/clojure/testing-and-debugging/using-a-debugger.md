---
date: 2024-01-26 03:48:27.123226-07:00
description: "\u4F7F\u7528\u8C03\u8BD5\u5668\u610F\u5473\u7740\u4F60\u7ED9\u81EA\u5DF1\
  \u914D\u5907\u4E86\u4E00\u4E2A\u653E\u5927\u955C\u6765\u5BA1\u67E5\u4F60\u7684\u4EE3\
  \u7801\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u6D88\u9664\u9519\
  \u8BEF\u3001\u7406\u89E3\u6D41\u7A0B\uFF0C\u5E76\u786E\u4FDD\u4ED6\u4EEC\u7684\u903B\
  \u8F91\u6309\u9884\u671F\u8FDB\u884C\u3002"
lastmod: '2024-03-11T00:14:21.076922-06:00'
model: gpt-4-0125-preview
summary: "\u4F7F\u7528\u8C03\u8BD5\u5668\u610F\u5473\u7740\u4F60\u7ED9\u81EA\u5DF1\
  \u914D\u5907\u4E86\u4E00\u4E2A\u653E\u5927\u955C\u6765\u5BA1\u67E5\u4F60\u7684\u4EE3\
  \u7801\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u6D88\u9664\u9519\
  \u8BEF\u3001\u7406\u89E3\u6D41\u7A0B\uFF0C\u5E76\u786E\u4FDD\u4ED6\u4EEC\u7684\u903B\
  \u8F91\u6309\u9884\u671F\u8FDB\u884C\u3002"
title: "\u4F7F\u7528\u8C03\u8BD5\u5668"
---

{{< edit_this_page >}}

## 什么 & 为什么？
使用调试器意味着你给自己配备了一个放大镜来审查你的代码。程序员这样做是为了消除错误、理解流程，并确保他们的逻辑按预期进行。

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
