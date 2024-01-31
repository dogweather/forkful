---
title:                "文字列の先頭を大文字にする"
date:                  2024-01-19
html_title:           "C: 文字列の先頭を大文字にする"
simple_title:         "文字列の先頭を大文字にする"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (なぜか？そして、何のために？)
文字列の大文字化とは、文字列内の全ての文字を大文字に変換することです。これは視認性を高めたり、定形化されたテキスト形式（例えば、ログファイルやデータエントリー）を確保するためにプログラマが行います。

## How to: (方法)
```PowerShell
# 文字列を大文字にする
$text = "こんにちは、PowerShell！"
$capitalizedText = $text.ToUpper()

# 出力
$capitalizedText
```

サンプル出力：

```
こんにちは、POWERSHELL！
```

## Deep Dive (深く探る)

PowerShellでの文字列の大文字化はシンプルだが、歴史は長いです。文字列を大文字にすることは、初期のコンピューティング時代からある操作です。大文字のみを扱う環境や、大文字が標準だった時代もあります。

`.ToUpper()` メソッドは .NET Frameworkに依存しています。これはPowerShellが.NETオブジェクトを扱えるようにしているためです。

他の方法としては、ASCIIコードを操作する古いスタイルのアプローチがありますが、モダンなコーディングでは推奨されていません。また、カルチャ固有の大文字変換を行う場合は、`.ToUpperInvariant()` を使うなどの選択肢もあります。

具体的な実装ですが、`.ToUpper()` は内部でUnicode文字の変換表を使用しており、国際化されたアプリケーションにはこれが適しています。

## See Also (関連情報)

- [Microsoft Docs: .ToUpper Method](https://docs.microsoft.com/en-us/dotnet/api/system.string.toupper)
- [Wikipedia: ASCII code](https://en.wikipedia.org/wiki/ASCII#Printable_characters)
