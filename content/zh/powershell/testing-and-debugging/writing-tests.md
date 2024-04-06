---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:37.535869-07:00
description: "\u5982\u4F55\u8FDB\u884C\uFF1A PowerShell \u6CA1\u6709\u5185\u7F6E\u7684\
  \u6D4B\u8BD5\u6846\u67B6\uFF0C\u4F46 Pester \u662F\u4E00\u4E2A\u5E7F\u53D7\u6B22\
  \u8FCE\u7684\u7B2C\u4E09\u65B9\u6A21\u5757\uFF0C\u5E38\u7528\u4E8E\u7F16\u5199\u548C\
  \u8FD0\u884C\u6D4B\u8BD5\u3002\u8FD9\u91CC\u662F\u5982\u4F55\u5F00\u59CB\u4F7F\u7528\
  \ Pester \u6765\u6D4B\u8BD5\u60A8\u7684 PowerShell \u51FD\u6570\u3002 \u9996\u5148\
  \uFF0C\u5982\u679C\u60A8\u8FD8\u6CA1\u6709\u5B89\u88C5 Pester\uFF0C\u8BF7\u5B89\u88C5\
  \uFF1A."
lastmod: '2024-04-05T21:53:48.314808-06:00'
model: gpt-4-0125-preview
summary: "\u9996\u5148\uFF0C\u5982\u679C\u60A8\u8FD8\u6CA1\u6709\u5B89\u88C5 Pester\uFF0C\
  \u8BF7\u5B89\u88C5\uFF1A."
title: "\u7F16\u5199\u6D4B\u8BD5"
weight: 36
---

## 如何进行：
PowerShell 没有内置的测试框架，但 Pester 是一个广受欢迎的第三方模块，常用于编写和运行测试。这里是如何开始使用 Pester 来测试您的 PowerShell 函数。

首先，如果您还没有安装 Pester，请安装：

```powershell
Install-Module -Name Pester -Scope CurrentUser -Force
```

接下来，假设您有一个想要测试的简单 PowerShell 函数，保存为 `MyFunction.ps1`：

```powershell
function Get-MultipliedNumber {
    param (
        [int]$Number,
        [int]$Multiplier = 2
    )

    return $Number * $Multiplier
}
```

要用 Pester 测试此功能，请创建一个名为 `MyFunction.Tests.ps1` 的测试脚本。在此脚本中，使用 Pester 的 `Describe` 和 `It` 块来定义测试用例：

```powershell
# 导入要测试的函数
. .\MyFunction.ps1

Describe "Get-MultipliedNumber 测试" {
    It "当没有提供乘数时，将数字乘以 2" {
        $result = Get-MultipliedNumber -Number 3
        $result | Should -Be 6
    }

    It "正确地将数字乘以给定的乘数" {
        $result = Get-MultipliedNumber -Number 3 -Multiplier 3
        $result | Should -Be 9
    }
}
```

要运行测试，请打开 PowerShell，导航到包含测试脚本的目录，并使用 `Invoke-Pester` 命令：

```powershell
Invoke-Pester .\MyFunction.Tests.ps1
```

示例输出将如下所示，指示您的测试是通过还是失败：

```
开始在 1 个文件中发现。
发现完成于 152ms。
[+] C:\path\to\MyFunction.Tests.ps1 204ms (182ms|16ms)
测试完成于 204ms
测试通过：2，失败：0，跳过：0 未运行：0
```

此输出显示两个测试都已通过，给您信心，说明您的 `Get-MultipliedNumber` 函数在您测试的场景下表现如预期。
