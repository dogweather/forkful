---
title:                "「HTTPリクエストの送信」"
html_title:           "Ruby: 「HTTPリクエストの送信」"
simple_title:         "「HTTPリクエストの送信」"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 何が可能なの？ 
HTTP リクエストとは、Web サーバーにデータを要求することを意味します。プログラマーは、情報の入出力が必要な場合や、Web サービスに接続する必要がある場合に使います。 

## 方法： 
```Ruby
require 'net/http'
uri = URI('https://example.com/') 
res = Net::HTTP.get_response(uri)
puts res.body 
``` 
出力： 
HTML ページのコンテンツ 

## 詳細を見る： 
HTTP リクエストは、Web 開発の伝説的なレガシー技術です。最近では、JavaScript のようなアプリケーションで代替手段が見つかったため、重要性は低くなっています。実装詳細を知りたい場合は、標準ライブラリの "net/http" モジュールをチェックしてください。 

## 関連情報を見る： 
- [Ruby 公式ドキュメント: Net::HTTP](https://docs.ruby-lang.org/en/3.0.0/Net/HTTP.html) 
- [プログラミング言語 Ruby メモ：HTTP リクエストの扱い方](https://programming-notes.hatenablog.com/entry/how-to-handle-an-http-request-in-ruby)