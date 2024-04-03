---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:37.535869-07:00
description: "\u7528 PowerShell \u7F16\u5199\u6D4B\u8BD5\u5305\u62EC\u521B\u5EFA\u811A\
  \u672C\uFF0C\u8FD9\u4E9B\u811A\u672C\u81EA\u52A8\u9A8C\u8BC1\u60A8\u7684 PowerShell\
  \ \u4EE3\u7801\u7684\u529F\u80FD\uFF0C\u786E\u4FDD\u5176\u8868\u73B0\u5982\u9884\
  \u671F\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u5C3D\u65E9\u53D1\
  \u73B0\u9519\u8BEF\uFF0C\u7B80\u5316\u4EE3\u7801\u7EF4\u62A4\uFF0C\u5E76\u786E\u4FDD\
  \u4EE3\u7801\u4FEE\u6539\u4E0D\u4F1A\u65E0\u610F\u4E2D\u7834\u574F\u73B0\u6709\u529F\
  \u80FD\u3002"
lastmod: '2024-03-13T22:44:48.017323-06:00'
model: gpt-4-0125-preview
summary: "\u7528 PowerShell \u7F16\u5199\u6D4B\u8BD5\u5305\u62EC\u521B\u5EFA\u811A\
  \u672C\uFF0C\u8FD9\u4E9B\u811A\u672C\u81EA\u52A8\u9A8C\u8BC1\u60A8\u7684 PowerShell\
  \ \u4EE3\u7801\u7684\u529F\u80FD\uFF0C\u786E\u4FDD\u5176\u8868\u73B0\u5982\u9884\
  \u671F\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u5C3D\u65E9\u53D1\
  \u73B0\u9519\u8BEF\uFF0C\u7B80\u5316\u4EE3\u7801\u7EF4\u62A4\uFF0C\u5E76\u786E\u4FDD\
  \u4EE3\u7801\u4FEE\u6539\u4E0D\u4F1A\u65E0\u610F\u4E2D\u7834\u574F\u73B0\u6709\u529F\
  \u80FD\u3002."
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
