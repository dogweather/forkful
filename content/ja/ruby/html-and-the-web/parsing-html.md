---
title:                "HTMLの解析"
aliases:
- /ja/ruby/parsing-html/
date:                  2024-02-03T19:13:08.071343-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTMLの解析"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？
HTMLのパースとは、HTMLコードの塊を分解してその構造と内容を理解することを意味します。プログラマーは、データを抽出したり、コンテンツを操作したり、フォーマットやシステム間で情報を移行するためにこれを行います。

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
