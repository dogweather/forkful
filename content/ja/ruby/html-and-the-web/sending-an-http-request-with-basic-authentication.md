---
date: 2024-01-20 18:02:39.270555-07:00
description: "How to: (\u65B9\u6CD5\uFF1A) \u5B9F\u884C\u7D50\u679C\u306E\u4F8B\uFF1A\
  ."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.643555-06:00'
model: gpt-4-1106-preview
summary: ''
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
