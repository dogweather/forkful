---
title:                "ウェブページのダウンロード"
html_title:           "Ruby: ウェブページのダウンロード"
simple_title:         "ウェブページのダウンロード"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Why
いろいろな理由で人々はウェブページをダウンロードする必要があります。例えば、ウェブサイトのデータをスクレイピングするため、オフラインでの閲覧のため、またはバックアップのためです。

## How To
ウェブページをダウンロードするためのRubyのコードは非常に簡単です。まずは`open-uri`ライブラリを`require`し、`open()`メソッドを使用してダウンロードしたいウェブページのURLを指定します。

```Ruby
require 'open-uri'

url = "https://www.example.com"
page = open(url)
```

このコードを実行すると、変数`page`にウェブページのデータが保存されます。その後、`read()`メソッドを使用してデータを読み込み、変数`html`に格納します。

```Ruby
html = page.read()
```

これでウェブページのデータを取得することができました。もちろん、ページを閲覧するのではなく、データを取得するだけであれば、`open()`メソッドの代わりに`open-uri`ライブラリの`URI()`メソッドを使用することもできます。

```Ruby
require 'open-uri'

url = "https://www.example.com"
html = URI(url).read()
```

### サンプルコード

```Ruby
require 'open-uri'

url = "https://www.example.com"
page = open(url)
html = page.read()

puts html
```

### 出力例

```html
<!DOCTYPE html>
<html>
<head>
  <title>Example Domain</title>
</head>
<body>
  <h1>Example Domain</h1>
  <p>This domain is for use in illustrative examples in documents. You may use this
  domain in literature without prior coordination or asking for permission.</p>
  <p><a href="https://www.iana.org/domains/example">More information...</a></p>
</body>
</html>
```

## Deep Dive
`open()`メソッドのみを使用すると、ウェブページのデータを文字列として取得することができます。しかし、`open-uri`ライブラリには便利なメソッドが多数用意されています。例えば、`open()`メソッドの代わりに`open-uri`ライブラリの`open()`メソッドを使用することで、画像や動画などのファイルを直接ダウンロードすることができます。

また、`Nokogiri`ライブラリを使用することで、取得したウェブページのデータを解析し、特定の要素を取得することもできます。

さらに、`open-uri`ライブラリを使用する際には、例外処理をしっかりと行うことが重要です。ウェブページが存在しないURLを指定した場合や、ネットワークエラーが発生した場合にはプログラムが正しく動作するように、適切なエラーハンドリングを行うようにしましょう。

## See Also
- [Ruby Documentation](https://docs.ruby-lang.org/en/)
- [Nokogiri Documentation](https://nokogiri.org/)
- [OpenURI Documentation](https://docs.ruby-lang.org/en/2.7.0/OpenURI.html)