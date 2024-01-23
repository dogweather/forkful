---
title:                "基本認証を使用したHTTPリクエストの送信"
date:                  2024-01-20T18:02:39.270555-07:00
model:                 gpt-4-1106-preview
simple_title:         "基本認証を使用したHTTPリクエストの送信"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)

HTTPリクエストにベーシック認証を付けるとは、ユーザー名とパスワードを使って安全な通信を確立することです。プログラマーはデータの保護のため、または限定されたアクセスを必要とするリソースに接続するためにこれを行います。

## How to: (方法：)

```Ruby
require 'net/http'
require 'uri'

uri = URI('http://example.com/secrets')
req = Net::HTTP::Get.new(uri)
req.basic_auth 'user', 'password'

res = Net::HTTP.start(uri.hostname, uri.port) {|http|
  http.request(req)
}

puts res.body
```

実行結果の例：

```
Secret information here!
```

## Deep Dive (掘り下げ)

ベーシック認証は、HTTP 1.0 で導入された古典的な認証方式です。リクエストのヘッダーに `Authorization` を追加し、ユーザー名とパスワードをBase64でエンコードした値を含めます。ただし、HTTPSを通じてのみ安全です。

代替案として、OAuth、トークンベース認証、APIキーなどの方法があります。これらは、二要素認証やスコープ限定アクセスなど、より進化したセキュリティを提供します。

Rubyでは `net/http` ライブラリを使うのが一般的ですが、`HTTParty` や `Faraday` などのライブラリも人気があります。これらは拡張性や使いやすさなど、独自の利点を持っています。

## See Also (関連情報)

- Ruby の標準ライブラリ `net/http`:[Ruby-Doc.org](https://ruby-doc.org/stdlib-3.0.0/libdoc/net/http/rdoc/Net/HTTP.html)
- HTTP認証に関する詳細:[MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- `HTTParty` gem:[GitHub](https://github.com/jnunemaker/httparty)
- `Faraday` gem:[GitHub](https://github.com/lostisland/faraday)
