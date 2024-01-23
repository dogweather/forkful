---
title:                "ランダム数の生成"
date:                  2024-01-20T17:49:39.102715-07:00
model:                 gpt-4-1106-preview
simple_title:         "ランダム数の生成"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Numbers"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
ランダム数生成は予測不可能な数値を作ることです。プログラマーはテストデータ作成、セキュリティ対策、ゲームの要素などに使用します。

## How to: (方法)
```PowerShell
# 1から100までのランダムな数
$randomNumber = Get-Random -Minimum 1 -Maximum 101
Write-Host "ランダム数: $randomNumber"

# 0と1の間でランダムな浮動小数点数
$randomFloat = Get-Random -Minimum 0 -Maximum 1 -AsDouble
Write-Host "ランダム浮動小数点数: $randomFloat"
```

出力例：
```
ランダム数: 47
ランダム浮動小数点数: 0.682687927246094
```

## Deep Dive (掘り下げ)
ランダム数生成は以前からありますが、初期の方法は完全にランダムとは限らなかったです。`System.Random` クラスは多くの.NETアプリケーションで使われていますが、PowerShell では `Get-Random` コマンドレットが簡単です。`Get-Random` は内部で疑似ランダム数を生成しますが、セキュリティが要求される場合は `System.Security.Cryptography.RandomNumberGenerator` クラスを利用するのが良いでしょう。また、ランダムシードを指定することで、疑似ランダムな挙動を再現可能にもできます。

## See Also (関連情報)
- [About Random](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-random?view=powershell-7.2): Microsoftの公式ドキュメントで `Get-Random` コマンドレットについての詳細。
- [.NET Random Class](https://docs.microsoft.com/en-us/dotnet/api/system.random?view=net-6.0): .NETの `System.Random` クラスに関する情報。
- [Cryptography in .NET](https://docs.microsoft.com/en-us/dotnet/standard/security/cryptography-model): .NETの暗号学について、よりセキュアなランダム数生成方法を含む。
