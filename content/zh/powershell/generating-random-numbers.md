---
title:                "生成随机数"
date:                  2024-01-27T20:35:02.915507-07:00
model:                 gpt-4-0125-preview
simple_title:         "生成随机数"

category:             "PowerShell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/powershell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
在 PowerShell 中生成随机数意味着在指定范围内创建不可预测的数值。程序员出于众多原因利用这一能力，包括测试、模拟和安全目的，其中不可预测性或模仿现实世界的随机性至关重要。

## 如何操作：
PowerShell 提供了一种简单的方法来使用 `Get-Random` cmdlet 生成随机数。这个 cmdlet 可以在默认范围内或指定范围内产生随机数。

```PowerShell
# 生成一个介于 0 和 Int32.MaxValue 之间的随机数
$randomNumber = Get-Random
Write-Output $randomNumber
```

要指定范围，请使用 `-Minimum` 和 `-Maximum` 参数：

```PowerShell
# 生成一个介于 1 到 100 之间的随机数
$randomNumber = Get-Random -Minimum 1 -Maximum 101
Write-Output $randomNumber
```

为了得到更多控制，您可以实例化一个 `System.Random` 类对象：

```PowerShell
# 使用 System.Random 生成一系列数字
$rand = New-Object System.Random
foreach ($i in 1..5) {
    $randomNumber = $rand.Next(1, 101)
    Write-Output $randomNumber
}
```

如果您需要从数组或集合中随机选择一个项目，`Get-Random` 可以直接选择一个项：

```PowerShell
# 从数组中随机选择
$array = 1..10
$randomItem = Get-Random -InputObject $array
Write-Output $randomItem
```

## 深入了解
PowerShell 中的 `Get-Random` cmdlet 利用 .NET 类 `System.Random` 来生成伪随机数。它们被称为“伪”是因为它们使用算法产生的数字序列只是看起来是随机的。对于大多数应用程序，这种随机性水平是足够的。然而，对于需要密码学安全的用例，`System.Random` 由于其可预测性并不适合。

PowerShell 和 .NET 提供 `System.Security.Cryptography.RNGCryptoServiceProvider` 用于密码学随机性，它更适合生成加密密钥或其他对安全性敏感的操作：

```PowerShell
# 密码学安全随机数
$rng = [System.Security.Cryptography.RNGCryptoServiceProvider]::new()
$bytes = New-Object byte[] 4
$rng.GetBytes($bytes)
$randomNumber = [BitConverter]::ToInt32($bytes, 0)
Write-Output $randomNumber
```

虽然 `Get-Random` 和 `System.Random` 满足了脚本和应用逻辑随机性的广泛需求，但在安全为中心的应用程序中特别需要选择合适的工具，因为可预测性可能呈现出脆弱性。
