---
aliases:
- /ja/powershell/parsing-html/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:52.314505-07:00
description: "\u2026"
lastmod: 2024-02-18 23:08:55.113036
model: gpt-4-0125-preview
summary: "\u2026"
title: "HTML\u306E\u89E3\u6790"
---

{{< edit_this_page >}}

## 何となぜ？
PowerShellでのHTMLパースとは、特定のデータを抽出するため、またはウェブ関連タスクを自動化するためにHTMLコンテンツを解析することです。プログラマーは、ウェブページと対話したり、ウェブコンテンツをスクレイピングしたり、フォームの送信や他のウェブインタラクションをウェブブラウザー無しで自動化したりするためにこれを行います。

## 方法：

PowerShellには専用のHTMLパーサーはネイティブにはありませんが、`Invoke-WebRequest` コマンドレットを使用してHTMLコンテンツにアクセスし、パースすることができます。より複雑なパースや操作には、人気のある.NETライブラリであるHtmlAgilityPackを使用することができます。

### `Invoke-WebRequest` の使用方法：

```powershell
# ウェブページからタイトルを取得する単純な例
$response = Invoke-WebRequest -Uri 'http://example.com'
# ParsedHtmlプロパティを利用してDOM要素にアクセス
$title = $response.ParsedHtml.title
Write-Output $title
```

サンプル出力：

```
Example Domain
```

### HtmlAgilityPackの使用方法：

まず、HtmlAgilityPackをインストールする必要があります。これはNuGetパッケージマネージャーを通じて行うことができます：

```powershell
Install-Package HtmlAgilityPack -ProviderName NuGet
```

その後、PowerShellでHTMLをパースするためにそれを使用できます：

```powershell
# HtmlAgilityPackアセンブリをロード
Add-Type -Path "path\to\HtmlAgilityPack.dll"

# HtmlDocumentオブジェクトを作成
$doc = New-Object HtmlAgilityPack.HtmlDocument

# ファイルまたはウェブリクエストからHTMLをロード
$htmlContent = (Invoke-WebRequest -Uri "http://example.com").Content
$doc.LoadHtml($htmlContent)

# XPathまたは他のクエリ方法を使用して要素を抽出
$node = $doc.DocumentNode.SelectSingleNode("//h1")

if ($node -ne $null) {
    Write-Output $node.InnerText
}
```

サンプル出力：

```
Example.comへようこそ！
```

これらの例では、`Invoke-WebRequest` は単純なタスクに最適ですが、HtmlAgilityPackは複雑なHTMLパースや操作に対してはるかに豊富な機能セットを提供します。
