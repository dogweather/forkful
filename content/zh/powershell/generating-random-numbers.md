---
title:                "生成随机数"
date:                  2024-01-20T17:49:45.176465-07:00
model:                 gpt-4-1106-preview
simple_title:         "生成随机数"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Numbers"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/powershell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
随机数生成是让计算机产生一个没有明显模式、不可预测数值的过程。程序员经常用它来测试软件、数据加密，或者在游戏开发中模拟随机事件。

## How to: (怎么做：)
PowerShell中生成随机数的基本方式是用`Get-Random` cmdlet。

```PowerShell
# 生成一个介于0到100之间的随机整数
Get-Random -Minimum 0 -Maximum 101

# 结果示例：42
```
你也可以生成随机字母串：
```PowerShell
# 生成一个长度为10的随机字母串
-join ((65..90) + (97..122) | Get-Random -Count 10 | ForEach-Object { [char]$_ })

# 结果示例：eXaMpLeStR
```

## Deep Dive (深入了解)
随机数生成在计算机历史中一直挺重要的。最早的计算机随机数是基于物理过程，比如放射性衰变。如今，大多数的随机数是“伪随机”，由算法产生。这种方法在重复性和效率上有好处，但不适合所有用途。比如，加密要求更高级别的不可预测性，就常用“硬件随机数生成器”。

PowerShell的`Get-Random`算法基于.NET `System.Random`类，适合一般用途，像脚本和自动化。然而，如果要密集地生成大量随机数，或需要很高的随机质量，可能要考虑其它语言或库。

另外，可用`Random`类直接实现更复杂的随机数功能：
```PowerShell
# 使用System.Random类
$rand = New-Object System.Random
$rand.Next(0, 101)

# 结果示例：58
```
记住，虽然这些数值看起来随机，但如果你使用相同的起始种子(`seed`)，`System.Random` 会产生相同的数列。

## See Also (另请参阅)
- 官方 `Get-Random` 文档: [Microsoft Docs](https://docs.microsoft.com/powershell/module/microsoft.powershell.utility/get-random)
- .NET `System.Random` 类: [Microsoft Docs](https://docs.microsoft.com/dotnet/api/system.random)
