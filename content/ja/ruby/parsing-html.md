---
title:                "HTMLの解析"
html_title:           "Ruby: HTMLの解析"
simple_title:         "HTMLの解析"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/parsing-html.md"
---

{{< edit_this_page >}}

## なぜ？
HTMLをパースする理由は多々あります。例えば、ウェブスクレイピングやスクレイピングされたデータを分析するためなどです。RubyはHTMLを簡単にパースできる強力な言語なので、これから紹介する方法を使ってHTMLをパースしてみましょう！

## パースする方法
HTMLをパースするのに必要なのは、Nokogiriというライブラリをインストールすることです。下記のコードをターミナルに入力してください。

```Ruby
gem install nokogiri
```

次に、HTMLをパースするためのコードを書いていきましょう。例として、ruby-lang.orgのトップページのタイトルを取得してみます。

```Ruby
require 'nokogiri'
require 'open-uri'

doc = Nokogiri::HTML(open("https://www.ruby-lang.org/"))
puts doc.at_css("title").text
```

上記のコードを実行すると、以下のような出力が得られます。

```
Ruby Programming Language
```

これで、HTMLからタイトルを取得することができました！

## もっと深く掘り下げる
もし、より複雑なHTMLをパースしなければならない場合は、CSSセレクターを使うことができます。Nokogiriでは、以下のようにCSSセレクターを使うことで、特定の要素を取得することができます。

```Ruby
doc.css("div#top-section .hero")
```

このコードは、idが"top-section"でクラスが"hero"のdiv要素を取得します。また、複数の要素を取得したい場合は、`.each`メソッドを使ってループ処理を行うこともできます。

また、NokogiriではXPathという言語を使ってもHTMLをパースすることができます。XPathは、より柔軟なクエリを書くことができるので、パースするHTMLによってはCSSセレクターよりも便利です。

## 参考リンク
- [Nokogiri公式ドキュメント](https://nokogiri.org/)
- [CSSセレクターガイド](https://www.w3schools.com/cssref/css_selectors.asp)
- [XPathチュートリアル](https://www.guru99.com/xpath-selenium.html)

## 参照
- [Nokogiriを使ったHTMLのパース](https://www.rubyguides.com/2018/10/nokogiri/)