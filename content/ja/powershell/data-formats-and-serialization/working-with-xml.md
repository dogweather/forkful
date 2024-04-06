---
date: 2024-01-26 04:34:23.947178-07:00
description: ''
lastmod: '2024-04-05T22:50:56.361000-06:00'
model: gpt-4-0125-preview
summary: "XML\u306E\u4EE3\u66FF\u306B\u306FJSON\u3001YAML\u3001\u30AB\u30B9\u30BF\u30E0\
  \u30C7\u30FC\u30BF\u5F62\u5F0F\u304C\u3042\u308A\u307E\u3059\u3002\u4F8B\u3048\u3070\
  \u3001JSON\u306F\u305D\u306E\u8EFD\u91CF\u306A\u6027\u8CEA\u3068\u30A6\u30A7\u30D6\
  \u30C6\u30AF\u30CE\u30ED\u30B8\u30FC\u3068\u306E\u4F7F\u3044\u3084\u3059\u3055\u304B\
  \u3089\u4EBA\u6C17\u3092\u535A\u3057\u3066\u3044\u307E\u3059\u3002\u3057\u304B\u3057\
  \u3001XML\u306E\u62E1\u5F35\u6A5F\u80FD\uFF08\u540D\u524D\u7A7A\u9593\u3001\u30B9\
  \u30AD\u30FC\u30DE\u3001XSLT\u51E6\u7406\u306A\u3069\uFF09\u306F\u3001\u8907\u96D1\
  \u306A\u30C9\u30AD\u30E5\u30E1\u30F3\u30C8\u3084\u696D\u754C\u6A19\u6E96\u306B\u306F\
  \u3088\u308A\u9069\u3057\u3066\u3044\u308B\u3053\u3068\u304C\u3088\u304F\u3042\u308A\
  \u307E\u3059\u3002"
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
