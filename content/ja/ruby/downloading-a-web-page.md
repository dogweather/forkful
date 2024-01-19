---
title:                "ウェブページのダウンロード"
html_title:           "Bash: ウェブページのダウンロード"
simple_title:         "ウェブページのダウンロード"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 何となぜ？

Webページをダウンロードするとは、WebサーバからHTMLファイル（画像やJavaScriptなども含む）を取得することを意味します。これは、データ分析、Webスクレイピング、自動化テストなどの目的でプログラマーが行います。

## やり方:

```Ruby
require 'open-uri'
require 'nokogiri'

url = 'https://example.com'
html = open(url)

doc = Nokogiri::HTML.parse(html.read)
doc.search('title').each do |title| 
  puts title.inner_text
end
```

上記のコードを実行すると、タイトルタグ内のテキストが出力されます。

##深堀り：

1) **歴史的な文脈**：Webページのダウンロードは、インターネットが人々の生活に浸透するようになった初期から行われてきました。初期のインターネットユーザーはダイヤルアップ接続を使ってWebページをダウンロードし、後にブラウザでオフラインで閲覧するために保存しました。

2) **代替手段**：Ruby以外にもPython、JavaScript（Node.js）、Javaなどのプログラミング言語でもWebページのダウンロードが可能です。使用言語は目的や好みによります。

3) **実装詳細**：Rubyでは、'open-uri'と'nokogiri'のライブラリを使用してWebページをダウンロードしていました。'open-uri'は指定したURLからデータを取得し、'nokogiri'はそのデータを解析・操作するためのものです。

##参照：

- Rubyの公式ドキュメンテーションで、この方についてより詳しく学ぶことができます：
  - 'open-uri': https://ruby-doc.org/stdlib-3.1.1/libdoc/open-uri/rdoc/OpenURI.html
  - 'nokogiri': https://nokogiri.org/
- Webスクレイピングについての基本：https://realpython.com/python-web-scraping-practical-introduction/
- RubyでのWebスクレイピングについての詳細な記事：https://www.rubyguides.com/2018/02/ruby-web-scraper/