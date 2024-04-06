---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:52.314505-07:00
description: "\u65B9\u6CD5\uFF1A PowerShell\u306B\u306F\u5C02\u7528\u306EHTML\u30D1\
  \u30FC\u30B5\u30FC\u306F\u30CD\u30A4\u30C6\u30A3\u30D6\u306B\u306F\u3042\u308A\u307E\
  \u305B\u3093\u304C\u3001`Invoke-WebRequest`\u2026"
lastmod: '2024-04-05T21:53:43.257041-06:00'
model: gpt-4-0125-preview
summary: ''
title: "HTML\u306E\u89E3\u6790"
weight: 43
---

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
