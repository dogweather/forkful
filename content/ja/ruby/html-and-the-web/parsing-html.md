---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:08.071343-07:00
description: "HTML\u306E\u30D1\u30FC\u30B9\u3068\u306F\u3001HTML\u30B3\u30FC\u30C9\
  \u306E\u584A\u3092\u5206\u89E3\u3057\u3066\u305D\u306E\u69CB\u9020\u3068\u5185\u5BB9\
  \u3092\u7406\u89E3\u3059\u308B\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30C7\u30FC\u30BF\u3092\u62BD\u51FA\
  \u3057\u305F\u308A\u3001\u30B3\u30F3\u30C6\u30F3\u30C4\u3092\u64CD\u4F5C\u3057\u305F\
  \u308A\u3001\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u3084\u30B7\u30B9\u30C6\u30E0\u9593\
  \u3067\u60C5\u5831\u3092\u79FB\u884C\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\
  \u884C\u3044\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.852013-06:00'
model: gpt-4-0125-preview
summary: "HTML\u306E\u30D1\u30FC\u30B9\u3068\u306F\u3001HTML\u30B3\u30FC\u30C9\u306E\
  \u584A\u3092\u5206\u89E3\u3057\u3066\u305D\u306E\u69CB\u9020\u3068\u5185\u5BB9\u3092\
  \u7406\u89E3\u3059\u308B\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30C7\u30FC\u30BF\u3092\u62BD\u51FA\u3057\
  \u305F\u308A\u3001\u30B3\u30F3\u30C6\u30F3\u30C4\u3092\u64CD\u4F5C\u3057\u305F\u308A\
  \u3001\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u3084\u30B7\u30B9\u30C6\u30E0\u9593\u3067\
  \u60C5\u5831\u3092\u79FB\u884C\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\
  \u3044\u307E\u3059\u3002."
title: "HTML\u306E\u89E3\u6790"
weight: 43
---

## 方法：
RubyでHTMLをパースするには、`gem install nokogiri`で'Nokogiri'ジェムをインストールします。Nokogiriは、RubyでHTMLとXMLを扱うためのスイスアーミーナイフのようなものです。こちらが簡単な例です：

```ruby
require 'nokogiri'
require 'open-uri'

# ウェブサイトからHTMLコンテンツを読み込む
html_content = URI.open('http://example.com').read

# HTMLをパースする
doc = Nokogiri::HTML(html_content)

# タイトルを抽出する
title = doc.xpath('//title').text
puts "ページのタイトルは：#{title}"
```

これは次のような結果を出力します：`ページのタイトルは：Example Domain`。

## 詳細解説
初期のRuby時代には、HTMLをパースする選択肢は限られていました。REXMLは組み込まれていましたが遅かったです。その後Hpricotが登場しましたが、それは消えてしまいました。Nokogiriは2008年に登場し、Hpricotの簡単さと、実績のあるXMLツールキットであるlibxmlの速度と力を組み合わせました。

パースの世界には常に代替品があります。一部の人々は、組み込みの'rexml'ライブラリや、Rubyの別のXML/HTMLパーサである'oga'を信じています。しかし、その堅牢性と速度、そして豊富な機能の配列で、Nokogiriはお気に入りのままです。

内部的には、NokogiriはHTMLをドキュメントオブジェクトモデル（DOM）—ツリー構造に変換します。これにより、要素を簡単にナビゲートして操作できます。XPathとCSSセレクタを使用して、必要な情報の任意の部分を特定できます。

## 参考
- Nokogiri gem: [https://nokogiri.org/](https://nokogiri.org/)
- Rubyのrexmlドキュメント：[https://ruby-doc.org/stdlib-2.6.3/libdoc/rexml/rdoc/REXML/Document.html](https://ruby-doc.org/stdlib-2.6.3/libdoc/rexml/rdoc/REXML/Document.html)
- 代替パーサー 'oga'：[https://github.com/YorickPeterse/oga](https://github.com/YorickPeterse/oga)
- XPathについて学ぶ：[https://www.w3schools.com/xml/xpath_intro.asp](https://www.w3schools.com/xml/xpath_intro.asp)
