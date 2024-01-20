---
title:                "文字列の連結"
html_title:           "Bash: 文字列の連結"
simple_title:         "文字列の連結"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/concatenating-strings.md"
---

{{< edit_this_page >}}

## 何？なぜ？

文字列の連結とは、一連の文字列を一つの文字列に連結することです。プログラマはこれを用いて、複数の情報を一つの文やメッセージに結合したり、複雑な文書を素早く作成するために使用します。

## 実施方法：

文字列の連結はPowerShellでは非常に簡単に行うことができます。たとえば以下のように+演算子を使用してます。

```PowerShell
$text1 = "こんにちは、"
$text2 = "世界！"
$greeting = $text1 + $text2
echo $greeting
```
出力:
```PowerShell
こんにちは、世界！
```
または「-f」フォーマット演算子を使用します：

```PowerShell
echo "{0} {1}" -f "こんにちは、","世界！"
```
出力:
```PowerShell
こんにちは、世界！
```

## 深掘り：

もともと、PowerShellはMicrosoftがWindowsシステム管理用に開発したコマンドラインツールです。文字列の連結は、最初からその一部として組み込まれていました。

代わりにStringBuilderクラスも使用することが可能です。コード上では少し複雑になりますが、パフォーマンスを改善する場合や大量のデータを扱う場合には有用です：

```PowerShell
$sb = New-Object System.Text.StringBuilder
$null = $sb.Append("こんにちは、")
$null = $sb.Append("世界！")
echo $sb.ToString()
```

また、注意すべき点として、`+`演算子を使用すると、新しい文字列を作成するたびに新しいメモリスペースが確保されます。このため、大量の文字列を連結するときはパフォーマンスに影響が出るかもしれません。

## 参考文献：

以下に、文字列連結についての追加情報を提供するリソースへのリンクを示します。

2. [StringBuilderクラス](https://docs.microsoft.com/ja-jp/dotnet/api/system.text.stringbuilder?view=netframework-4.8)