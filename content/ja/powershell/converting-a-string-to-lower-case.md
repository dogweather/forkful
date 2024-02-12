---
title:                "文字列を小文字に変換"
aliases:
- ja/powershell/converting-a-string-to-lower-case.md
date:                  2024-01-20T17:39:02.138334-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列を小文字に変換"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? / 何となぜ？
文字列を小文字に変換するとは、アルファベット大文字を小文字にして統一する処理のことです。大文字と小文字を区別せずにデータを比較したり、一貫性を保つためにプログラマーはこれを行います。

## How to: / 方法
PowerShellで文字列を小文字に変換するには、`.ToLower()` メソッドを使います。以下に例を示します。

```PowerShell
# 文字列を定義する
$string = "Hello, World!"

# 文字列を小文字に変換する
$lowerCaseString = $string.ToLower()

# 結果を表示する
$lowerCaseString
```

これは次のような出力を生成します。

```
hello, world!
```

## Deep Dive / 詳細情報
文字列を小文字に変換する処理は、文字列の正規化の一部です。歴史的には、大文字と小文字が区別され始めたのは、印刷技術が発展した中世にさかのぼります。プログラミングにおいて、様々な文化や言語環境で文字列を扱う場合、.NET Frameworkの`ToLower()` メソッドは、カルチャに依存する方法と依存しない方法の両方を提供します。カルチャ非依存の変換には`.ToLowerInvariant()`が使われます。

ほかの方法としては、`[string]::ToLower()` スタティックメソッドがありますが、通常`.ToLower()` メソッドで十分です。注意点として、特定の文字はカルチャによって小文字変換の挙動が異なることがあるため、国際化されたアプリケーションでの使用では慎重に選択する必要があります。

## See Also / 関連情報
- `.NET` の文字列操作についての詳細: [Microsoft Docs](https://docs.microsoft.com/dotnet/api/system.string.tolower)
- 文字列の大文字と小文字を区別しない比較に関する情報: [Microsoft Docs](https://docs.microsoft.com/dotnet/standard/base-types/best-practices-strings#comparing)
