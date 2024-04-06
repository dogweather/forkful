---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:08.071343-07:00
description: "\u65B9\u6CD5\uFF1A Ruby\u3067HTML\u3092\u30D1\u30FC\u30B9\u3059\u308B\
  \u306B\u306F\u3001`gem install nokogiri`\u3067'Nokogiri'\u30B8\u30A7\u30E0\u3092\
  \u30A4\u30F3\u30B9\u30C8\u30FC\u30EB\u3057\u307E\u3059\u3002Nokogiri\u306F\u3001\
  Ruby\u3067HTML\u3068XML\u3092\u6271\u3046\u305F\u3081\u306E\u30B9\u30A4\u30B9\u30A2\
  \u30FC\u30DF\u30FC\u30CA\u30A4\u30D5\u306E\u3088\u3046\u306A\u3082\u306E\u3067\u3059\
  \u3002\u3053\u3061\u3089\u304C\u7C21\u5358\u306A\u4F8B\u3067\u3059\uFF1A."
lastmod: '2024-04-05T22:38:42.342435-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A Ruby\u3067HTML\u3092\u30D1\u30FC\u30B9\u3059\u308B\u306B\
  \u306F\u3001`gem install nokogiri`\u3067'Nokogiri'\u30B8\u30A7\u30E0\u3092\u30A4\
  \u30F3\u30B9\u30C8\u30FC\u30EB\u3057\u307E\u3059\u3002Nokogiri\u306F\u3001Ruby\u3067\
  HTML\u3068XML\u3092\u6271\u3046\u305F\u3081\u306E\u30B9\u30A4\u30B9\u30A2\u30FC\u30DF\
  \u30FC\u30CA\u30A4\u30D5\u306E\u3088\u3046\u306A\u3082\u306E\u3067\u3059\u3002\u3053\
  \u3061\u3089\u304C\u7C21\u5358\u306A\u4F8B\u3067\u3059\uFF1A."
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
