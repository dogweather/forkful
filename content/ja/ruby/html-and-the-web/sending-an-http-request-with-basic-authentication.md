---
date: 2024-01-20 18:02:39.270555-07:00
description: "How to: (\u65B9\u6CD5\uFF1A) \u30D9\u30FC\u30B7\u30C3\u30AF\u8A8D\u8A3C\
  \u306F\u3001HTTP 1.0 \u3067\u5C0E\u5165\u3055\u308C\u305F\u53E4\u5178\u7684\u306A\
  \u8A8D\u8A3C\u65B9\u5F0F\u3067\u3059\u3002\u30EA\u30AF\u30A8\u30B9\u30C8\u306E\u30D8\
  \u30C3\u30C0\u30FC\u306B `Authorization` \u3092\u8FFD\u52A0\u3057\u3001\u30E6\u30FC\
  \u30B6\u30FC\u540D\u3068\u30D1\u30B9\u30EF\u30FC\u30C9\u3092Base64\u3067\u30A8\u30F3\
  \u30B3\u30FC\u30C9\u3057\u305F\u5024\u3092\u542B\u3081\u307E\u3059\u3002\u305F\u3060\
  \u3057\u3001HTTPS\u3092\u901A\u3058\u3066\u306E\u307F\u5B89\u5168\u3067\u3059\u3002\
  \u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:50:56.724869-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5\uFF1A) \u30D9\u30FC\u30B7\u30C3\u30AF\u8A8D\u8A3C\u306F\u3001\
  HTTP 1.0 \u3067\u5C0E\u5165\u3055\u308C\u305F\u53E4\u5178\u7684\u306A\u8A8D\u8A3C\
  \u65B9\u5F0F\u3067\u3059\u3002\u30EA\u30AF\u30A8\u30B9\u30C8\u306E\u30D8\u30C3\u30C0\
  \u30FC\u306B `Authorization` \u3092\u8FFD\u52A0\u3057\u3001\u30E6\u30FC\u30B6\u30FC\
  \u540D\u3068\u30D1\u30B9\u30EF\u30FC\u30C9\u3092Base64\u3067\u30A8\u30F3\u30B3\u30FC\
  \u30C9\u3057\u305F\u5024\u3092\u542B\u3081\u307E\u3059\u3002\u305F\u3060\u3057\u3001\
  HTTPS\u3092\u901A\u3058\u3066\u306E\u307F\u5B89\u5168\u3067\u3059\u3002"
title: "\u57FA\u672C\u8A8D\u8A3C\u3092\u4F7F\u7528\u3057\u305FHTTP\u30EA\u30AF\u30A8\
  \u30B9\u30C8\u306E\u9001\u4FE1"
weight: 45
---

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
