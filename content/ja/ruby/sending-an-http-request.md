---
title:                "HTTPリクエストの送信"
html_title:           "Bash: HTTPリクエストの送信"
simple_title:         "HTTPリクエストの送信"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 何となぜ?

HTTPリクエストの送信とは、一言で言うと、あるプログラムがインターネットを通じて他のプログラムにデータを要求することです。プログラマがこれを行う主な理由は、情報を取得したり、特定のサービスを利用したり、サービス間でデータを共有したりするためです。

## どうやって:

RubyでHTTPリクエストを送信するための基本的なコードは以下の通りです。

```Ruby
require 'net/http'
require 'uri'

uri = URI.parse("http://www.example.com/search")
response = Net::HTTP.get_response(uri)

puts response.body
```

上記のコードを実行すると、"http://www.example.com/search" URLに対するHTTPレスポンスボディが出力されます。

## Deep Dive

HTTPリクエストの送信は、初期のインターネットから存在しています。そしてその技術は、Rubyに限らず、ほぼ全ての現代のプログラミング言語で利用できます。

Rubyでは 'net/http' ライブラリを使ってHTTPリクエストを送信しますが、他のライブラリやGemを用いても可能です。例えば 'httparty' や 'rest-client' といったGemがよく利用されます。

上記のコード例では `GET` リクエストを行いましたが、 `POST`, `PUT` , `DELETE` など他のHTTPメソッドを利用する場合も多々あります。それぞれの方法でリクエストを送信する詳細なコードは以下の公式ドキュメンテーションを参照してください。

## 参考に

以下は、HTTP リクエスト、'net/http' ライブラリ、そして他の関連するリソースへのリンクです。

- [Ruby公式ドキュメンテーション: Net::HTTP] (https://ruby-doc.org/stdlib-2.7.0/libdoc/net/http/rdoc/Net/HTTP.html)
- [RubyGuide: HTTPリクエストの扱い] (https://www.rubyguides.com/2018/08/net-http-ruby/)
- [HTTParty Gem] (https://github.com/jnunemaker/httparty)
- [REST-Client Gem] (https://github.com/rest-client/rest-client)