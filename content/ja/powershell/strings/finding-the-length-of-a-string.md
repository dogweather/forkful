---
title:                "文字列の長さを求める"
aliases:
- /ja/powershell/finding-the-length-of-a-string/
date:                  2024-01-20T17:48:26.574116-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列の長さを求める"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列の長さを見つけるとは、その文字列に含まれる文字の数を数えることです。プログラマはデータの検証、テキスト処理、ユーザー入力の管理などの目的でこれを行います。

## How to: (方法)
PowerShellで文字列の長さを見つけるには、`.Length`プロパティを使います。

```PowerShell
$text = "こんにちは、世界！"
$textLength = $text.Length
$textLength
```

サンプル出力：

```
9
```

この例では、`$text`の文字列の長さが`9`と表示されます。これには日本語の文字も含まれています。

## Deep Dive (詳細な情報)
PowerShellで`.Length`プロパティを使うと、.NETの`System.String`クラスを介して文字列の長さを求めます。歴史的には、多くのプログラミング言語は長さや他の文字列操作をサポートする独自の機能を持ちます。`.Length`はPowerShellにおける直感的で簡単な方法の一つですが、他にも`$text.ToCharArray().Count`や`$text | Measure-Object -Character`のような方法があります。

文字列の長さを見つける時の実装詳細：
- Unicode: PowerShellはUnicodeをサポートしており、全ての文字を適切に数えます。
- 文字列の終端: PowerShellでの文字列は終端ナル文字を必要としません。

これらの違いは異なる環境や要求に適応するために重要です。

## See Also (関連リンク)
- [about_Automatic_Variables](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_automatic_variables?view=powershell-7.1) - PowerShellの自動変数に関する公式ドキュメント。
- [about_Properties](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_properties?view=powershell-7.1) - PowerShellのプロパティに関する公式ドキュメント。
