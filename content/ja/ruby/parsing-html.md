---
title:                "Ruby: HTMLの解析"
simple_title:         "HTMLの解析"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/parsing-html.md"
---

{{< edit_this_page >}}

## なぜ

HTMLを解析することに何か意味があるの？プログラマーにとっては、情報を全ての形に分解するようなものです。例えば、Webスクレイピングやデータ収集などのタスクを実行するときに便利です。

## 使い方

RailsやSinatraなどのRubyフレームワークを使用している場合、HTML解析のための便利なライブラリがあります。NokogiriやMechanizeなどのライブラリを使用して、簡単にHTMLを解析することができます。

例えば、以下のコードを使用すると、指定したURLからHTMLを取得し、特定の要素を抽出することができます。

```Ruby
require 'nokogiri'
require 'open-uri'

doc = Nokogiri::HTML(URI.open("https://example.com/"))
puts doc.css("h1").text # h1タグのテキストを出力
```

出力結果は以下のようになります。

```
RubyのHTML解析
```

## ディープダイブ

HTML解析には、タグやクラスなどの識別子を使用して要素を特定する方法があります。また、XPathやCSSセレクターを使用することもできます。

さらに、Nokogiriを使用してHTMLを変更することも可能です。例えば、`.css`を`.xpath`に変更することで、XPathを使用して要素を取得することができます。

```Ruby
require 'nokogiri'
require 'open-uri'

doc = Nokogiri::HTML(URI.open("https://example.com/"))
puts doc.xpath("//h1").text # h1タグのテキストを出力
```

出力結果は先ほどと同じです。

## 併せて読みたい

- [Ruby on Rails Tutorial: 文字列操作とデータ処理](https://railstutorial.jp/chapters/string_operations.html)
- [Nokogiriの公式ドキュメント](https://nokogiri.org/)
- [Mechanizeの公式ドキュメント](https://github.com/sparklemotion/mechanize)