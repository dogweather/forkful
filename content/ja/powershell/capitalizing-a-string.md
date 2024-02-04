---
title:                "文字列を大文字にする"
date:                  2024-02-03T19:06:13.144344-07:00
model:                 gpt-4-0125-preview
simple_title:         "文字列を大文字にする"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？
PowerShellで文字列を大文字化するとは、与えられた文字列の最初の文字を大文字に変換し、残りの文字列を変更しないままにすることを指します。プログラマーは、ユーザーインターフェースでのテキスト表示の準備や、生成されたドキュメントの文法規則に従うためなど、フォーマット目的でこのタスクをよく実行します。

## 方法：
PowerShellは多機能なツールであり、サードパーティのライブラリを必要とせずに、直接的な方法で文字列を大文字化することができます。以下の方法で実現できます：

```powershell
# .Netの組み込みメソッド「ToTitleCase」をCultureInfoから使用
$text = "hello world"
$culture = [System.Globalization.CultureInfo]::InvariantCulture
$capitalizedText = $culture.TextInfo.ToTitleCase($text.ToLower())
Write-Output $capitalizedText
```
出力：
```
Hello world
```

注：この方法は各単語の最初の文字を大文字にします。文字列の最初の文字のみを大文字にして、残りをそのままにしたい場合は、以下のようにすることができます：

```powershell
# 文字列の最初の文字のみを大文字化
$text = "hello world"
$capitalizedText = $text.Substring(0,1).ToUpper() + $text.Substring(1)
Write-Output $capitalizedText
```
出力：
```
Hello world
```

PowerShellは、文字列の最初の文字のみを大文字にするためのシンプルな関数を直接には含んでいませんが、`Substring(0,1).ToUpper()`のような基本的な文字列操作メソッドと結合を組み合わせることで、希望する結果を簡単に実現できます。
