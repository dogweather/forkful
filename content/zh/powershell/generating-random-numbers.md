---
title:                "生成随机数"
html_title:           "Go: 生成随机数"
simple_title:         "生成随机数"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/powershell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 什么 & 为什么? 
生成随机数的本质是创建一个不可预见的序列。它在编程中广泛用于数据科学,游戏,安全等领域。

## 如何:
在 PowerShell 中，我们使用 Get-Random Cmdlet 来生成随机数。让我们来了解一下其基本应用。

```PowerShell
#生成一个介于 0 到 100 之间的随机数
Get-Random -Maximum 100   
```
执行上述代码，PowerShell 会生成一个随机数。

```PowerShell
#生成一个介于 50 到 100 之间的随机数
Get-Random -Minimum 50 -Maximum 100
```

## 深入研究
随机数生成的概念可以追溯到古老的数学。然而，在计算机程序中，真正的随机性难以实现，因为计算机是由人类设计的确定性系统。生成的随机数通常被称为 "伪随机数"。

在 PowerShell 中，Get-Random cmdlet 基于.NET的 System.Random 类生成伪随机数。尽管这不是一个加密安全的随机数生成器，但对于多数日常任务来说足够好。

对于需要更强安全性的随机数，PowerShell 可使用 RNGCryptoServiceProvider 类，这是一个基于密码的伪随机数生成器。例如:

```PowerShell
# 先创建一个 byte 数组
$bytes = New-Object Byte[] 4
# 创建 RNGCryptoServiceProvider 对象
$rng = New-Object Security.Cryptography.RNGCryptoServiceProvider
# 把随机数生成到 byte 数组中
$rng.GetBytes($bytes)
# 转换 bytes 到 Int32
$random = [System.BitConverter]::ToInt32($bytes, 0)
```

## 另请参阅
以下是关于 Powershell 和随机数生成的一些有用资源：
- PowerShells Get-Random Cmdlet: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-random?view=powershell-7.1
- RNGCryptoServiceProvider Class: https://docs.microsoft.com/en-us/dotnet/api/system.security.cryptography.rngcryptoserviceprovider?view=net-5.0
- PowerShell 官方文档: https://docs.microsoft.com/en-us/powershell/