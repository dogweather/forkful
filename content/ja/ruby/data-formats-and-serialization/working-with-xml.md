---
date: 2024-01-26 04:35:29.174145-07:00
description: "\u65B9\u6CD5 Ruby\u306B\u542B\u307E\u308C\u308BREXML\u3092\u4F7F\u7528\
  \u3057\u3066\u3001XML\u30B9\u30CB\u30DA\u30C3\u30C8\u3092\u89E3\u6790\u3057\u3066\
  \u307F\u307E\u3057\u3087\u3046\uFF1A."
lastmod: '2024-03-13T22:44:42.888407-06:00'
model: gpt-4-0125-preview
summary: "Ruby\u306B\u542B\u307E\u308C\u308BREXML\u3092\u4F7F\u7528\u3057\u3066\u3001\
  XML\u30B9\u30CB\u30DA\u30C3\u30C8\u3092\u89E3\u6790\u3057\u3066\u307F\u307E\u3057\
  \u3087\u3046\uFF1A."
title: "XML\u306E\u6271\u3044\u65B9"
weight: 40
---

## 方法
Rubyに含まれるREXMLを使用して、XMLスニペットを解析してみましょう：
```Ruby
require 'rexml/document'
include REXML

xml_data = <<-XML
<fruits>
  <fruit name="apple" color="green"/>
  <fruit name="banana" color="yellow"/>
</fruits>
XML

document = Document.new(xml_data)
document.elements.each('fruits/fruit') do |element|
  puts "名前: #{element.attributes['name']}, 色: #{element.attributes['color']}"
end
```
出力：
```
名前: apple, 色: green
名前: banana, 色: yellow
```

XMLの生成も同様に簡単です：
```Ruby
doc = Document.new
doc.add_element 'fruits'
apple = doc.root.add_element 'fruit', {'name' => 'apple', 'color' => 'green'}
banana = doc.root.add_element 'fruit', {'name' => 'banana', 'color' => 'yellow'}
puts doc
```
XML出力：
```XML
<fruits>
  <fruit name="apple" color="green"/>
  <fruit name="banana" color="yellow"/>
</fruits>
```

## 詳細：
XMLの起源は1990年代にさかのぼり、ウェブドキュメント用のSGMLの簡易化したサブセットとして始まりました。冗長ですが非常に構造化されており、それが定着した理由です。JSONやYAMLがそのシンプルさから人気を集めていますが、XMLは多くの企業やレガシーシステムで強力に残っています。

RubyはXMLを扱うためのいくつかの方法を提供します。REXMLはすぐに始められる全Rubyライブラリです。Nokogiriは、速度と追加機能を提供する、より高速なCライブラリをラップするジェムです。どちらを選ぶかですが、小規模なタスクにはREXMLから始め、もっとパワーが必要な場合はNokogiriに移行すると良いでしょう。

内部では、XMLの解析は文字列をDOMまたはSAXモデルへの変換についてです。DOMはメモリ内にツリーを作成し、SAXはドキュメントをストリームし、解析するにつれてイベントを発生させます。REXMLは両方のモデルを提供しますが、Nokogiriが使用するC拡張機能よりも遅い傾向があります。

## 参照
- Ruby REXMLドキュメント：https://www.rubydoc.info/stdlib/rexml
- Nokogiriジェム：https://nokogiri.org/
- XML仕様：https://www.w3.org/XML/
- SAXの紹介：https://www.saxproject.org/
- YAMLとJSONとXMLの比較：https://www.upwork.com/resources/json-vs-xml-vs-yaml
