---
title:                "打印调试输出"
html_title:           "Clojure: 打印调试输出"
simple_title:         "打印调试输出"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/powershell/printing-debug-output.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？

控制台打印调试输出是在控制台打印出程序运行过程的各种信息。程序员做这件事的目的是更好地了解并调试程序的运行情况。

## 如何操作：

```PowerShell
Write-Debug "Debug info here" -Debug
Write-Verbose "Verbose info here" -Verbose
```
当你运行上述代码时，调试和详细信息将会显示出来。

## 深入学习：

#### 历史背景：
Print语句的历史可以追溯到早期的编程语言，如Fortran和C。PowerShell改进了这个概念，提供了丰富多样的输出选项。

#### 替代品：
除了Write-Debug, Write-Verbose之外，还有Write-Warning、Write-Error、Write-Output、Write-Host等命令, 它们分别用于输出不同级别的信息。

#### 实现细节：
Write-Debug并不像其它的Write语句那样直接进行打印，需要 -Debug 开关，而不仅仅是依赖$DebugPreference变量。类似地, Write-Verbose也需要 -Verbose 开关。

## 参看:

1. 更多的关于Write-Debug和Write-Verbose的学习资源可以访问PowerShell文档: https://docs.microsoft.com/powershell/module/microsoft.powershell.utility/write-debug
2. 想要掌握更多的关于PowerShell的元数据命令，可以查阅这篇文章: https://docs.microsoft.com/powershell/scripting/learn/deep-dives/everything-about-string-comparisons
3. 想要掌握更多的关于调试PowerShell脚本的知识，可以查阅这篇文章：https://docs.microsoft.com/powershell/scripting/learn/debugging-from-command-line