---
date: 2024-01-26 04:34:23.947178-07:00
description: "XML\u3092\u6271\u3046\u3053\u3068\u306F\u3001eXtensible Markup\u2026"
lastmod: '2024-03-13T22:44:42.469097-06:00'
model: gpt-4-0125-preview
summary: "XML\u3092\u6271\u3046\u3053\u3068\u306F\u3001eXtensible Markup Language\u3067\
  \u69CB\u9020\u5316\u3055\u308C\u305F\u30C7\u30FC\u30BF\u3092\u64CD\u4F5C\u304A\u3088\
  \u3073\u30A2\u30AF\u30BB\u30B9\u3059\u308B\u3053\u3068\u3092\u610F\u5473\u3057\u307E\
  \u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u4ED6\u306E\u30B7\u30B9\
  \u30C6\u30E0\u3068\u306E\u76F8\u4E92\u904B\u7528\u3092\u53EF\u80FD\u306B\u3059\u308B\
  \u305F\u3081\u3084\u3001\u8A2D\u5B9A\u30D5\u30A1\u30A4\u30EB\u3001\u30C7\u30FC\u30BF\
  \u30D5\u30A3\u30FC\u30C9\u3001\u30A6\u30A7\u30D6\u30B5\u30FC\u30D3\u30B9\u3067\u4E00\
  \u822C\u7684\u306A\u305D\u306E\u4ED6\u306E\u69CB\u9020\u5316\u30C9\u30AD\u30E5\u30E1\
  \u30F3\u30C8\u306E\u8AAD\u307F\u66F8\u304D\u3092\u3059\u308B\u305F\u3081\u306BXML\u3092\
  \u4F7F\u7528\u3057\u307E\u3059\u3002."
title: "XML\u306E\u6271\u3044\u65B9"
weight: 40
---

## 方法：
```PowerShell
# XMLファイルを変数に読み込む
[xml]$xmlContent = Get-Content 'path\to\your\file.xml'

# XMLノードにアクセスする
$books = $xmlContent.catalog.book
foreach ($book in $books) {
  Write-Output "Title: $($book.title)"
}

# 新しいXML要素を作成する
$newBook = $xmlContent.CreateElement("book")
$newBook.SetAttribute("id", "bk999")
$xmlContent.DocumentElement.AppendChild($newBook)

# XMLをファイルに戻して保存する
$xmlContent.Save('path\to\your\updated\file.xml')
```
サンプル出力：
```
Title: Programming PowerShell
Title: XML Essentials
```

## 深堀り
XML、またはeXtensible Markup Languageは90年代後半から存在しており、構造化データのための広く使用されている形式として残っています。PowerShellは伝統的な解析方法と比較してXMLを扱いやすくします。直接オブジェクトにXMLをキャストし、要素を馴染みのあるドット表記を使って操作できます。

XMLの代替にはJSON、YAML、カスタムデータ形式があります。例えば、JSONはその軽量な性質とウェブテクノロジーとの使いやすさから人気を博しています。しかし、XMLの拡張機能（名前空間、スキーマ、XSLT処理など）は、複雑なドキュメントや業界標準にはより適していることがよくあります。

PowerShellは、そのXMLの扱いに.NET FrameworkのXML機能を使用しています。これは、単純な読み書き操作だけでなく、スキーマによる検証、XPathによるクエリ、XSLT変換をPowerShellを通して行うことができることを意味します。

## 参照
- [W3Schools XMLチュートリアル](https://www.w3schools.com/xml/)
- [XML vs. JSON](https://www.json.org/json-en.html)
