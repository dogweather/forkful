---
title:                "HTMLの解析"
date:                  2024-01-20T15:33:28.465931-07:00
html_title:           "Arduino: HTMLの解析"
simple_title:         "HTMLの解析"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
HTMLパースとは、HTMLコードを解析してデータを抽出することです。プログラマはウェブスクレイピング、データマイニング、またはコンテンツ管理システムでのデータ変換のためにこれを行います。

## How to: (方法)
PowerShellを使用してHTMLをパースする基本的なサンプルです。

```PowerShell
# Invoke-WebRequestを使ってHTMLを取得
$response = Invoke-WebRequest -Uri 'http://example.com'

# HtmlAgilityPackを用いてHTMLをロード
Add-Type -Path "path\to\HtmlAgilityPack.dll"
$htmlDoc = New-Object HtmlAgilityPack.HtmlDocument
$htmlDoc.LoadHtml($response.Content)

# XPathを用いて特定の要素を見つける
$nodes = $htmlDoc.DocumentNode.SelectNodes('//h1')

# 要素の内容を出力
foreach ($node in $nodes) {
  Write-Output $node.InnerText
}
```

実行結果はウェブページにあるすべてのh1タグのテキストを出力します。

## Deep Dive (深掘り)
HTMLをパースするには、かつては正規表現が使われましたが、今では専用のライブラリやツールが一般的です。HtmlAgilityPackは.NETでよく使われるHTMLパースライブラリの一つです。XMLパースと同様にXPathやCSSセレクターを使うことで、特定のHTML要素に簡単にアクセスできます。内部では、HTMLをDOMツリーに変換して操作を行います。

代替として、AngleSharpなどのモダンなライブラリがあります。実装の詳細に関しては、HTMLの解析にはHTML5の仕様に準拠しているパーサーが望ましいですが、ウェブ上のHTMLが常に標準に沿っているわけではないため、柔軟性も重要です。

## See Also (関連リンク)
- [HtmlAgilityPack GitHub Repository](https://github.com/zzzprojects/html-agility-pack)
- [Invoke-WebRequest documentation](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest)
- [XPath Syntax](https://www.w3schools.com/xml/xpath_syntax.asp)
- [AngleSharp GitHub Repository](https://github.com/AngleSharp/AngleSharp)
