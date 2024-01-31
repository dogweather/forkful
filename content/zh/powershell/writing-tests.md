---
title:                "编写测试代码"
date:                  2024-01-19
html_title:           "Arduino: 编写测试代码"
simple_title:         "编写测试代码"

category:             "PowerShell"
tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/powershell/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (是什么 & 为什么？)
编写测试是创建用来自动检查代码功能是否正确的脚本工作。程序员这么做可以确保代码修改不会导致不期望的行为。

## How to: (如何操作)
PowerShell使用Pester框架来编写测试。下面是一个小例子，测试一个简单的函数能否正确返回“Hello, World”。

```PowerShell
function Get-Greeting {
    return "Hello, World"
}

Describe "Get-Greeting" {
    It "returns 'Hello, World'" {
        Get-Greeting | Should -Be "Hello, World"
    }
}
```

运行这个测试，输出应该如下：

```PowerShell
Describing Get-Greeting
 [+] returns 'Hello, World' 82ms
```

## Deep Dive (深入了解)
测试编写的历史开始于软件开发早期；它的实践随着TDD（测试驱动开发）成为标准进程而蓬勃发展。在PowerShell中，除了Pester，还可以使用PSUnit等其他框架。编写PowerShell测试时，常涉及模拟复杂函数或命令的操作，这个时候，Pester提供了Mock功能，使得独立于实际执行环境的测试成为可能。

## See Also (另见)
- 官方Pester文档: [Pester Documentation](https://pester.dev)
- PowerShell测试指南: [PowerShell Testing Guide](https://github.com/pester/Pester/wiki)
- 测试驱动开发(TDD)介绍: [Introduction to Test Driven Development (TDD)](https://www.agilealliance.org/glossary/tdd/)
