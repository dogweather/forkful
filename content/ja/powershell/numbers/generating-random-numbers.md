---
title:                "乱数の生成"
aliases:
- /ja/powershell/generating-random-numbers.md
date:                  2024-01-27T20:35:21.759795-07:00
model:                 gpt-4-0125-preview
simple_title:         "乱数の生成"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## はじめに：何となぜ？
PowerShellで乱数を生成することは、特定の範囲内で予測不可能な数値を作り出すことに関わります。プログラマーは、テスト、シミュレーション、セキュリティ目的など、予測不可能性が重要であるまたは実世界のランダム性を模倣する場合など、様々な理由でこの機能を利用します。

## 方法：
PowerShellでは、`Get-Random` コマンドレットを使用して乱数を簡単に生成できます。このコマンドレットは、デフォルトの範囲または指定された範囲内で乱数を生成することができます。

```PowerShell
# 0からInt32.MaxValueまでの間の乱数を生成
$randomNumber = Get-Random
Write-Output $randomNumber
```

範囲を指定するには、`-Minimum` と `-Maximum` パラメーターを使用します：

```PowerShell
# 1から100までの間の乱数を生成
$randomNumber = Get-Random -Minimum 1 -Maximum 101
Write-Output $randomNumber
```

より細かい制御が必要な場合は、`System.Random` クラスのオブジェクトをインスタンス化できます：

```PowerShell
# 数列のために System.Random を使用
$rand = New-Object System.Random
foreach ($i in 1..5) {
    $randomNumber = $rand.Next(1, 101)
    Write-Output $randomNumber
}
```

配列やコレクションからランダムな選択が必要な場合、`Get-Random` は直接アイテムを選ぶことができます：

```PowerShell
# 配列からのランダムな選択
$array = 1..10
$randomItem = Get-Random -InputObject $array
Write-Output $randomItem
```

## 詳細解説
PowerShellの `Get-Random` コマンドレットは、基盤となる.NET クラス `System.Random` を利用して疑似乱数を生成します。これらは「疑似」であるため、ランダムであると見せかける数列を生成するアルゴリズムを使用します。ほとんどのアプリケーションにおいて、このレベルのランダム性で十分です。しかし、暗号学的セキュリティを必要とする用途では、`System.Random` はその予測可能な性質のため不適切です。

PowerShellと.NETは、暗号化キー生成やその他のセキュリティに敏感な操作に適したより安全な `System.Security.Cryptography.RNGCryptoServiceProvider` を提供しています：

```PowerShell
# 暗号学的に安全な乱数
$rng = [System.Security.Cryptography.RNGCryptoServiceProvider]::new()
$bytes = New-Object byte[] 4
$rng.GetBytes($bytes)
$randomNumber = [BitConverter]::ToInt32($bytes, 0)
Write-Output $randomNumber
```

`Get-Random` と `System.Random` はスクリプティングやアプリケーションロジックのためのランダム性の広範なニーズを満たしていますが、予測可能性が脆弱性を生じさせうるセキュリティ中心のアプリケーションでは、適切なツールを選択することが不可欠です。
