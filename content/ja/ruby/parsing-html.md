---
title:                "「html の解析」"
html_title:           "Ruby: 「html の解析」"
simple_title:         "「html の解析」"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/parsing-html.md"
---

{{< edit_this_page >}}

# 何？ & なぜ？

プログラマーは、コンピューターで作成されたウェブページから情報を抽出することができるように、HTMLを解析する必要があります。解析とは、HTMLの構造や要素を理解し、必要なデータを取得することです。

## 使い方：

下記のコード例では、Rubyを使用してHTMLを解析し、タグ内の内容を抽出し、コンソールに出力します。更に、特定の属性やクラス名を指定して、より具体的なデータを取得することもできます。

```
require 'nokogiri'

html = "<html><body><h1>Hello World</h1><p>This is a sample paragraph.</p></body></html>"

doc = Nokogiri::HTML(html)

puts doc.css('h1').text #=> "Hello World"

puts doc.css('p').text #=> "This is a sample paragraph."

puts doc.css('body').text #=> "Hello WorldThis is a sample paragraph."
```

## 詳しく見てみよう：

HTMLを解析するための代表的なライブラリとして、NokogiriやMechanizeがあります。これらのライブラリは、ウェブスクレイピングやデータ収集などの用途でよく使用されています。

また、HTMLを解析する方法には正規表現を使用する方法もありますが、これは複雑なパターンのHTMLを解析する際にはあまり適していません。

Nokogiriの場合、libxmlと呼ばれるライブラリを使用してHTMLの解析を行っています。libxmlは高速でパワフルな解析エンジンであり、Nokogiriを通じて簡単に利用することができます。

## 関連サイトをチェック：

- [Nokogiri公式ドキュメント](https://nokogiri.org/)
- [Mechanize公式ドキュメント](https://mechanize.readthedocs.io/en/latest/)
- [libxml公式サイト](http://xmlsoft.org/)