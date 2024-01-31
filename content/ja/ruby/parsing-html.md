---
title:                "HTMLの解析"
date:                  2024-01-20T15:33:46.117022-07:00
simple_title:         "HTMLの解析"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
HTMLパースは、HTMLドキュメントからデータを取得・解析することです。プログラマーは情報を抽出したり、Webスクレイピングを行ったりするためにこれを行います。

## How to (やり方)
RubyではNokogiriというライブラリを使って簡単にHTMLをパースできます。インストールしてみましょう。

```ruby
gem install nokogiri
```

以下は基本的な使い方です。

```ruby
require 'nokogiri'
require 'open-uri'

# HTMLを読み込む
doc = Nokogiri::HTML(URI.open('http://example.com'))

# タイトルを取得
title = doc.css('title').first.content
puts title # => "Example Domain"

# リンクを全て取得
links = doc.css('a').map { |link| link['href'] }
puts links # => ["http://www.iana.org/domains/example"]
```

このコードは、まずNokogiriを使ってHTMLを読み込み、タイトルタグの内容と全てのリンクを取得しています。

## Deep Dive (深掘り)
Nokogiriは、2008年にリリースされたRubyのライブラリです。パースのスピードが速く、多くの開発者に信頼されています。

他にもオプションはありますが、Nokogiriは文書操作が容易で、CSSセレクタやXPathをサポートしています。内部的には、Nokogiriはlibxml2というXMLのCライブラリを利用していて、そのためパフォーマンスが良好です。

処理速度をさらに向上させたい場合は、HTMLをパースする前に不要な内容を削除する等の前処理を行うことが可能です。

## See Also (関連情報)
- Nokogiriの公式ドキュメント: [http://nokogiri.org/](http://nokogiri.org/)
- Rubyのダウンロードとインストールガイド: [https://www.ruby-lang.org/ja/downloads/](https://www.ruby-lang.org/ja/downloads/)
- Webスクレイピングの法的側面: [https://www.eff.org/issues/coders rights](https://www.eff.org/issues/coders-rights)

注意: Webスクレイピングは対象サイトの利用規約や法律を遵守する必要があります。使用前に確認しましょう。
