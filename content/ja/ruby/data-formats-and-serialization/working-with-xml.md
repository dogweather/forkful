---
date: 2024-01-26 04:35:29.174145-07:00
description: "XML\u3092\u6271\u3046\u3053\u3068\u306F\u3001XML\uFF08eXtensible Markup\
  \ Language\uFF09\u30C9\u30AD\u30E5\u30E1\u30F3\u30C8\u3092\u30B3\u30FC\u30C9\u3092\
  \u4F7F\u7528\u3057\u3066\u89E3\u6790\u3001\u751F\u6210\u3001\u304A\u3088\u3073\u64CD\
  \u4F5C\u3059\u308B\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u30D7\u30ED\
  \u30B0\u30E9\u30DE\u30FC\u306F\u3001\u591A\u304F\u306EWeb\u30B5\u30FC\u30D3\u30B9\
  \u3001\u8A2D\u5B9A\u30D5\u30A1\u30A4\u30EB\u3001\u305D\u3057\u3066XML\u304C\u5171\
  \u901A\u8A00\u8A9E\u3067\u3042\u308B\u30C7\u30FC\u30BF\u4EA4\u63DB\u30D5\u30A9\u30FC\
  \u30DE\u30C3\u30C8\u3068\u3084\u308A\u53D6\u308A\u3059\u308B\u305F\u3081\u306B\u3053\
  \u308C\u3092\u884C\u3044\u307E\u3059\u3002"
lastmod: '2024-02-25T18:49:40.818792-07:00'
model: gpt-4-0125-preview
summary: "XML\u3092\u6271\u3046\u3053\u3068\u306F\u3001XML\uFF08eXtensible Markup\
  \ Language\uFF09\u30C9\u30AD\u30E5\u30E1\u30F3\u30C8\u3092\u30B3\u30FC\u30C9\u3092\
  \u4F7F\u7528\u3057\u3066\u89E3\u6790\u3001\u751F\u6210\u3001\u304A\u3088\u3073\u64CD\
  \u4F5C\u3059\u308B\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u30D7\u30ED\
  \u30B0\u30E9\u30DE\u30FC\u306F\u3001\u591A\u304F\u306EWeb\u30B5\u30FC\u30D3\u30B9\
  \u3001\u8A2D\u5B9A\u30D5\u30A1\u30A4\u30EB\u3001\u305D\u3057\u3066XML\u304C\u5171\
  \u901A\u8A00\u8A9E\u3067\u3042\u308B\u30C7\u30FC\u30BF\u4EA4\u63DB\u30D5\u30A9\u30FC\
  \u30DE\u30C3\u30C8\u3068\u3084\u308A\u53D6\u308A\u3059\u308B\u305F\u3081\u306B\u3053\
  \u308C\u3092\u884C\u3044\u307E\u3059\u3002"
title: "XML\u306E\u6271\u3044\u65B9"
---

{{< edit_this_page >}}

## 何となぜ？
XMLを扱うことは、XML（eXtensible Markup Language）ドキュメントをコードを使用して解析、生成、および操作することを意味します。プログラマーは、多くのWebサービス、設定ファイル、そしてXMLが共通言語であるデータ交換フォーマットとやり取りするためにこれを行います。

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
