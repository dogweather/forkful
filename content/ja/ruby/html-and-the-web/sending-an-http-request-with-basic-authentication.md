---
date: 2024-01-20 18:02:39.270555-07:00
description: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306B\u30D9\u30FC\u30B7\u30C3\u30AF\
  \u8A8D\u8A3C\u3092\u4ED8\u3051\u308B\u3068\u306F\u3001\u30E6\u30FC\u30B6\u30FC\u540D\
  \u3068\u30D1\u30B9\u30EF\u30FC\u30C9\u3092\u4F7F\u3063\u3066\u5B89\u5168\u306A\u901A\
  \u4FE1\u3092\u78BA\u7ACB\u3059\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u30FC\u306F\u30C7\u30FC\u30BF\u306E\u4FDD\u8B77\u306E\u305F\u3081\u3001\
  \u307E\u305F\u306F\u9650\u5B9A\u3055\u308C\u305F\u30A2\u30AF\u30BB\u30B9\u3092\u5FC5\
  \u8981\u3068\u3059\u308B\u30EA\u30BD\u30FC\u30B9\u306B\u63A5\u7D9A\u3059\u308B\u305F\
  \u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.854736-06:00'
model: gpt-4-1106-preview
summary: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306B\u30D9\u30FC\u30B7\u30C3\u30AF\u8A8D\
  \u8A3C\u3092\u4ED8\u3051\u308B\u3068\u306F\u3001\u30E6\u30FC\u30B6\u30FC\u540D\u3068\
  \u30D1\u30B9\u30EF\u30FC\u30C9\u3092\u4F7F\u3063\u3066\u5B89\u5168\u306A\u901A\u4FE1\
  \u3092\u78BA\u7ACB\u3059\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u306F\u30C7\u30FC\u30BF\u306E\u4FDD\u8B77\u306E\u305F\u3081\u3001\u307E\
  \u305F\u306F\u9650\u5B9A\u3055\u308C\u305F\u30A2\u30AF\u30BB\u30B9\u3092\u5FC5\u8981\
  \u3068\u3059\u308B\u30EA\u30BD\u30FC\u30B9\u306B\u63A5\u7D9A\u3059\u308B\u305F\u3081\
  \u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002."
title: "\u57FA\u672C\u8A8D\u8A3C\u3092\u4F7F\u7528\u3057\u305FHTTP\u30EA\u30AF\u30A8\
  \u30B9\u30C8\u306E\u9001\u4FE1"
weight: 45
---

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
