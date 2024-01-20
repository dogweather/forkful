---
title:                "HTMLの解析"
html_title:           "Arduino: HTMLの解析"
simple_title:         "HTMLの解析"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/parsing-html.md"
---

{{< edit_this_page >}}

## 何となぜ？（What & Why?）

HTMLバース（Parsing HTML）とは、HTMLドキュメントを構成要素に分解して理解するプロセスのことです。これは、ウェブスクレイピングを行ったり、ウェブページの内容を分析したりするためによく使われます。

## どうやって？（How to）

Rubyでは、`Nokogiri`ライブラリを使用してHTMLを解析（パース）できます。以下がサンプルコードです。

```ruby
require 'nokogiri'
require 'open-uri'

doc = Nokogiri::HTML(open('http://www.example.com'))

doc.css('h1').each do |header|
  puts header.text
end
```

上記のコードは`www.example.com`の各`h1`タグのテキストを出力します。

## 深掘り（Deep Dive）

HTMLパースの歴史は、ウェブの成長とともに深まってきました。当初は簡単な正規表現を使用してHTMLタグをマッチさせるだけだったのが、IE5の頃にはDOM（Document Object Model）が公式に確立され、XMLやHTMLの解析が一段と容易になりました。

現在では、Ruby以外のプログラムでもHTML解析ライブラリが利用可能です。Pythonの`BeautifulSoup`やJavascriptの`cheerio`などが例として挙げられます。

また、Nokogiri自体はcライブラリ`libxml2`と`libxslt`のラッパーとなっています。技術的には、ウェブページの読み込みは`open-uri`によって、HTMLの解析は`libxml2`によって行われています。

## 参照（See Also）

- Nokogiri公式ドキュメント：http://nokogiri.org/
- HTMLパースの歴史について：https://www.quora.com/What-is-the-history-of-HTML-parsing
- libxml2公式サイト：http://xmlsoft.org/