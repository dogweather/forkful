---
date: 2024-01-27 20:35:21.759795-07:00
description: "PowerShell\u3067\u4E71\u6570\u3092\u751F\u6210\u3059\u308B\u3053\u3068\
  \u306F\u3001\u7279\u5B9A\u306E\u7BC4\u56F2\u5185\u3067\u4E88\u6E2C\u4E0D\u53EF\u80FD\
  \u306A\u6570\u5024\u3092\u4F5C\u308A\u51FA\u3059\u3053\u3068\u306B\u95A2\u308F\u308A\
  \u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30C6\u30B9\u30C8\
  \u3001\u30B7\u30DF\u30E5\u30EC\u30FC\u30B7\u30E7\u30F3\u3001\u30BB\u30AD\u30E5\u30EA\
  \u30C6\u30A3\u76EE\u7684\u306A\u3069\u3001\u4E88\u6E2C\u4E0D\u53EF\u80FD\u6027\u304C\
  \u91CD\u8981\u3067\u3042\u308B\u307E\u305F\u306F\u5B9F\u4E16\u754C\u306E\u30E9\u30F3\
  \u30C0\u30E0\u6027\u3092\u6A21\u5023\u3059\u308B\u5834\u5408\u306A\u3069\u3001\u69D8\
  \u3005\u306A\u7406\u7531\u3067\u3053\u306E\u6A5F\u80FD\u3092\u5229\u7528\u3057\u307E\
  \u3059\u3002"
lastmod: '2024-03-13T22:44:42.428796-06:00'
model: gpt-4-0125-preview
summary: "PowerShell\u3067\u4E71\u6570\u3092\u751F\u6210\u3059\u308B\u3053\u3068\u306F\
  \u3001\u7279\u5B9A\u306E\u7BC4\u56F2\u5185\u3067\u4E88\u6E2C\u4E0D\u53EF\u80FD\u306A\
  \u6570\u5024\u3092\u4F5C\u308A\u51FA\u3059\u3053\u3068\u306B\u95A2\u308F\u308A\u307E\
  \u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30C6\u30B9\u30C8\u3001\
  \u30B7\u30DF\u30E5\u30EC\u30FC\u30B7\u30E7\u30F3\u3001\u30BB\u30AD\u30E5\u30EA\u30C6\
  \u30A3\u76EE\u7684\u306A\u3069\u3001\u4E88\u6E2C\u4E0D\u53EF\u80FD\u6027\u304C\u91CD\
  \u8981\u3067\u3042\u308B\u307E\u305F\u306F\u5B9F\u4E16\u754C\u306E\u30E9\u30F3\u30C0\
  \u30E0\u6027\u3092\u6A21\u5023\u3059\u308B\u5834\u5408\u306A\u3069\u3001\u69D8\u3005\
  \u306A\u7406\u7531\u3067\u3053\u306E\u6A5F\u80FD\u3092\u5229\u7528\u3057\u307E\u3059\
  \u3002."
title: "\u4E71\u6570\u306E\u751F\u6210"
weight: 12
---

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
