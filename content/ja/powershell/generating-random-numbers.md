---
title:                "乱数の生成"
html_title:           "PowerShell: 乱数の生成"
simple_title:         "乱数の生成"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 何をする & なぜする?

ランダムな数字を生成することは、プログラマーにとって頻繁に利用されるタスクです。ランダムな数字は、ゲームの結果を決めるためや、テストデータを作成するためなど、さまざまな用途に使われます。

## 方法:

```PowerShell
# 0-10のランダムな数字を生成する
Get-Random -Minimum 0 -Maximum 10
 
# ランダムな文字列を生成する
[System.Web.Security.Membership]::GeneratePassword(8, 0)
```

生成された数字や文字列は、ターミナルに直接表示されます。このようにして、簡単にランダムな値を生成することができます。

## 深堀り:

ランダムな数字を生成する方法はさまざまあります。PowerShellの`Get-Random`コマンドレットは、比較的新しい方法ですが、以前から利用されてきた方法もあります。たとえば、.NET Frameworkの`System.Random`クラスを使う方法もあります。

また、ランダムな数字を生成するアルゴリズムは、セキュリティにも重要です。不十分なアルゴリズムを使うと、予測されやすい数字が生成されてしまい、セキュリティ上の問題を引き起こす可能性があります。そのため、セキュリティに配慮したランダムな値の生成を行う場合、より高度な方法が必要になります。

## 関連リンク:

- Get-Random (https://docs.microsoft.com/ja-jp/powershell/module/microsoft.powershell.utility/get-random)
- System.Random クラス (https://docs.microsoft.com/ja-jp/dotnet/api/system.random)
- ランダムな数字の生成についての詳細 (https://www.computerhope.com/jargon/g/genenum.htm)