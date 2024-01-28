---
title:                "文字列から引用符を削除する"
date:                  2024-01-26T03:42:01.385548-07:00
model:                 gpt-4-0125-preview
simple_title:         "文字列から引用符を削除する"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## はじめに：何となく理由
PowerShellで文字列からクォートを削除すると、テキストの周りを囲んでいる単一（`'`）または二重（`"`）引用符を取り除きます。特にユーザーの入力やファイルのパースを扱う場合、プログラマーは処理、比較、または出力の目的で文字列をクリーンアップする必要がよくあります。

## 方法：
文字列からクォートを削除するために、`-replace` 演算子を使用できます。以下の方法で行います：

```PowerShell
# 単一引用符を置換
$stringWithSingleQuotes = "'Hello, World!'"
$cleanString = $stringWithSingleQuotes -replace "'", ""
Write-Output $cleanString  # 出力：Hello, World!

# 二重引用符を置換
$stringWithDoubleQuotes = '"Hello, World!"'
$cleanString = $stringWithDoubleQuotes -replace '"', ""
Write-Output $cleanString  # 出力：Hello, World!
```

両方のタイプに対して：

```PowerShell
$stringWithQuotes = '"Hi there," she said.'
$cleanString = $stringWithQuotes -replace "[\"']", ""  # 正規表現キャラクタークラスの使用に注目
Write-Output $cleanString  # 出力：Hi there, she said.
```

コンソールからのサンプル出力は以下のようになります：

```
Hello, World!
Hello, World!
Hi there, she said.
```

## 詳細情報
以前は、PowerShellがMicrosoftの計画の一部でさえなかった時代に、Windowsでのテキスト処理はしばしば機能に限りがあるバッチスクリプトの範疇でした。PowerShellの導入により、強力な文字列操作機能がもたらされ、スクリプト作成がはるかに堅牢なものとなりました。

`-replace` に代わる方法も存在しますが、例えば `.Trim()` メソッドを使用して文字列の開始と終了のクォートのみを削除することができますが、同じ制御や正規表現のサポートを提供するわけではありません。

```PowerShell
# 開始と終了のクォートに対して .Trim() を使用
$stringWithQuotes = '"Hello, World!"'
$cleanString = $stringWithQuotes.Trim('"')
Write-Output $cleanString  # 出力：Hello, World!
```

注記：`-replace`は裏で正規表現を使用しているので、それを扱う際には、対象とする特別な文字がある場合はエスケープする必要があることに注意してください。より詳細なクォートの削除制御が必要な場合、`-replace` を使って正規表現に深く潜ることで、大幅な柔軟性を得ることができます。

## 関連情報
- PowerShellでの正規表現についての詳細は、公式ドキュメントを参照してください：[about_Regular_Expressions](https://docs.microsoft.com/ja-jp/powershell/module/microsoft.powershell.core/about/about_regular_expressions?view=powershell-7.1)
- 他の文字列メソッドを発見する：[Trim(), TrimStart(), TrimEnd()](https://docs.microsoft.com/ja-jp/dotnet/api/system.string.trim?view=net-6.0)
