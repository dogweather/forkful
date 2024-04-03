---
date: 2024-01-26 04:34:23.947178-07:00
description: "\u65B9\u6CD5\uFF1A ."
lastmod: '2024-03-13T22:44:42.469097-06:00'
model: gpt-4-0125-preview
summary: .
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
