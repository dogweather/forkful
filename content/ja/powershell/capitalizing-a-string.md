---
title:                "文字列を大文字にする"
html_title:           "PowerShell: 文字列を大文字にする"
simple_title:         "文字列を大文字にする"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？
文字列を大文字にするとは、一文字の小文字を対応する大文字に変換することを意味します。プログラマーは、ユーザー入力を正規化したり、表示形式を一貫させるためにこれを利用します。

## どうやる？
PowerShellでは、文字列を大文字に変換するための一種の方法は `ToUpper()` メソッドを使用することです。以下にその使用例を示します:

```PowerShell
$lowerCaseString = "hello, world"
$upperCaseString = $lowerCaseString.ToUpper()
```

このコードを実行すると、 `$upperCaseString` は `"HELLO, WORLD"` となります。

## ディープダイブ
歴史的な背景としては、大文字への変換は古くからあり、各種プログラミング言語で標準的な組み込み関数として提供されています。
それに対する代替策としては、例えば `mb_strtoupper` 関数などがPHPで利用可能です。しかし、PowerShellでは乱暴ですが、ASCII文字だけを大文字に変換することも可能です。

これが具体的な実装の細部になります:

```PowerShell
$lowerCaseString = "hello, world"
$upperCaseString = $lowerCaseString | ForEach-Object { if([int][char]$_ -ge 97 -and [int][char]$_ -le 122) { [char]([int][char]$_ - 32) } else { $_ } }
```

ただし、この方法はASCII文字の範囲外の文字には対応していません。そのため、非ASCII文字でも安全に大文字変換を行う場合は安全な方を推奨します、すなわち `ToUpper()` メソッドです。

## 参考資料
1. [MSDN 文字列操作のメソッド](https://docs.microsoft.com/ja-jp/dotnet/api/system.string?view=net-5.0)
2. [PowerShell 文字列操作のチュートリアル](https://ss64.com/ps/syntax-operators.html)