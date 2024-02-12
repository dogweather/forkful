---
title:                "XMLの扱い方"
aliases:
- /ja/ruby/working-with-xml.md
date:                  2024-01-26T04:35:29.174145-07:00
model:                 gpt-4-0125-preview
simple_title:         "XMLの扱い方"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/working-with-xml.md"
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
