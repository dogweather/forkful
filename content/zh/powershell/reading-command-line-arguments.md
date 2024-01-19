---
title:                "读取命令行参数"
html_title:           "C: 读取命令行参数"
simple_title:         "读取命令行参数"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/powershell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 什么和为什么?

命令行参数能够让您的脚本更可灵活，通过输入不同的参数，让您的脚本得以执行不同的任务。这使得你的程序变得更加适应于水到渠成的用例，并增加了代码的重复利用率。

## 如何操作：

在 PowerShell 中，脚本可以通过 "$args" 变量来获取命令行参数，该变量是一个数组，存放了所有输入的参数。这是一个简单的例子：

```PowerShell
# test.ps1 
write-output "参数列表: $args"
```
现在，我们运行这个脚本并给它一些参数：
```PowerShell
PS C:\> ./test.ps1 arg1 arg2 arg3
```

输出是：
```PowerShell
参数列表: arg1 arg2 arg3
```
每个参数都设定在 "$args" 变量里面，并顺序输出。

## 深入探索

历史背景：原始 Unix shell (Bourne shell) 也有一个和 PowerShell 中的 "$args" 很相似的概念。这个传统被继承并在许多现代的 shell 和脚本语言（包括 PowerShell, bash 和 Python）等等中得以保留。

替代方法：除了 "$args" 之外，"param" 关键字也可以用来规定命令行参数。"param" 能使命令行参数在脚本中的定义变得更明确，也能规定默认值和类型。

实施细节："$args" 是一个数组，它的索引从 0 开始，这意味着 "$args[0]" 指向的是第一个参数，"$args[1]" 是第二个，以此类推。

## 参考资料

1. [PowerShell 官方参数文档](https://docs.microsoft.com/zh-cn/powershell/module/microsoft.powershell.core/about/about_parameters?view=powershell-7.1)
2. [PowerShell 官方 "$args" 变量文档](https://docs.microsoft.com/zh-cn/powershell/module/microsoft.powershell.core/about/about_automatic_variables?view=powershell-7.1#args)
3. [关于 PowerShell 和 Unix shell 参数的比较](https://adamtheautomator.com/powershell-args/)