---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:52.314505-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:42.433914-06:00'
model: gpt-4-0125-preview
summary: "PowerShell\u3067\u306EHTML\u30D1\u30FC\u30B9\u3068\u306F\u3001\u7279\u5B9A\
  \u306E\u30C7\u30FC\u30BF\u3092\u62BD\u51FA\u3059\u308B\u305F\u3081\u3001\u307E\u305F\
  \u306F\u30A6\u30A7\u30D6\u95A2\u9023\u30BF\u30B9\u30AF\u3092\u81EA\u52D5\u5316\u3059\
  \u308B\u305F\u3081\u306BHTML\u30B3\u30F3\u30C6\u30F3\u30C4\u3092\u89E3\u6790\u3059\
  \u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\
  \u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u3068\u5BFE\u8A71\u3057\u305F\u308A\u3001\u30A6\
  \u30A7\u30D6\u30B3\u30F3\u30C6\u30F3\u30C4\u3092\u30B9\u30AF\u30EC\u30A4\u30D4\u30F3\
  \u30B0\u3057\u305F\u308A\u3001\u30D5\u30A9\u30FC\u30E0\u306E\u9001\u4FE1\u3084\u4ED6\
  \u306E\u30A6\u30A7\u30D6\u30A4\u30F3\u30BF\u30E9\u30AF\u30B7\u30E7\u30F3\u3092\u30A6\
  \u30A7\u30D6\u30D6\u30E9\u30A6\u30B6\u30FC\u7121\u3057\u3067\u81EA\u52D5\u5316\u3057\
  \u305F\u308A\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\
  \u3002."
title: "HTML\u306E\u89E3\u6790"
weight: 43
---

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
