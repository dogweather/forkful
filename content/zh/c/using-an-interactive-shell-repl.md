---
title:                "在编程中使用交互式Shell（REPL）"
date:                  2024-01-26T04:11:47.301355-07:00
model:                 gpt-4-0125-preview
simple_title:         "在编程中使用交互式Shell（REPL）"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## 什么和为什么？
交互式shell，或者叫读取-求值-输出循环（REPL），是一种提供实时编程环境的工具，可以立即测试代码片段。程序员在开发、学习和调试过程中使用它以获得快速反馈。

## 如何使用：
C语言本身并没有内置的REPL，但你可以使用第三方工具。这里用 Cling 来举个例子，Cling 是一个C++解释器，也能够处理C代码：

```C
#include <stdio.h>

int main() {
    printf("Hello, REPL world!\n");
    return 0;
}
```

在 Cling REPL 中的输出：
```
[cling]$ .x yourscript.c
Hello, REPL world!
```

Cling 执行脚本并立即打印输出。

## 深入了解
在像Python或Ruby这样的动态语言中，REPLs 是标准配置，但对于像C这样的编译型语言，它们就不太常见。历史上，编译-运行-调试的周期并不适合交互式探索。Cling 等工具和在线C编译器通过将你的C代码包装在一个C++环境中，提供了类似REPL的体验。

Cling的替代品包括C解释器，如CINT和Ch。这些工具允许快速迭代，但由于性能限制和对复杂特性的支持，它们可能不适用于所有开发场景。

在编译型语言中实现REPL涉及到即时编译和执行代码片段，这是非常繁杂的，而且与完整语言能力相比可能会有限制。

## 参见
- Cling: https://github.com/root-project/cling
- 在线C编译器和REPL: https://repl.it/languages/c
- CINT: http://root.cern.ch/drupal/content/cint
- Ch解释器: http://www.softintegration.com/products/chstandard/
