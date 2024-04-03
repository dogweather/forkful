---
date: 2024-01-20 18:00:21.459666-07:00
description: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\u308B\u3063\u3066\u3044\
  \u3046\u306E\u306F\u3001Web\u30B5\u30FC\u30D0\u30FC\u306B\u60C5\u5831\u3092\u8981\
  \u6C42\u3059\u308B\u65B9\u6CD5\u3060\u3002\u3053\u308C\u3092\u3059\u308B\u7406\u7531\
  \uFF1F\u30C7\u30FC\u30BF\u3092\u53D6\u5F97\u3057\u305F\u308A\u3001\u30B5\u30FC\u30D3\
  \u30B9\u3068\u5BFE\u8A71\u3057\u305F\u308A\u3059\u308B\u305F\u3081\u3060\u3088\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.850592-06:00'
model: gpt-4-1106-preview
summary: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\u308B\u3063\u3066\u3044\u3046\
  \u306E\u306F\u3001Web\u30B5\u30FC\u30D0\u30FC\u306B\u60C5\u5831\u3092\u8981\u6C42\
  \u3059\u308B\u65B9\u6CD5\u3060\u3002\u3053\u308C\u3092\u3059\u308B\u7406\u7531\uFF1F\
  \u30C7\u30FC\u30BF\u3092\u53D6\u5F97\u3057\u305F\u308A\u3001\u30B5\u30FC\u30D3\u30B9\
  \u3068\u5BFE\u8A71\u3057\u305F\u308A\u3059\u308B\u305F\u3081\u3060\u3088\u3002."
title: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306E\u9001\u4FE1"
weight: 44
---

## What & Why? (何となぜ？)
HTTPリクエストを送るっていうのは、Webサーバーに情報を要求する方法だ。これをする理由？データを取得したり、サービスと対話したりするためだよ。

## How to: (やり方)
RubyではNet::HTTPを使ってHTTPリクエストを送れる。こんな感じ：

```ruby
require 'net/http'
require 'uri'

uri = URI('http://example.com/index.html')
response = Net::HTTP.get(uri)

puts response
```

実行すると、サーバーのレスポンスが表示される。

## Deep Dive (深掘り)
まず、戦前からHTTPプロトコルはWebの中核だ。`Net::HTTP`はRuby標準ライブラリの一部。もしこれが物足りなければ、`httparty`や`rest-client`なんかのgemもある。`Net::HTTP`では、GET, POST, PUT, DELETEなどのリクエストが可能。SSLやプロキシの設定もできる。

サンプルに加えたい詳細:

```ruby
require 'net/http'
require 'uri'

uri = URI('https://example.com/api/items')
http = Net::HTTP.new(uri.host, uri.port)
http.use_ssl = true

request = Net::HTTP::Get.new(uri)
request['Authorization'] = 'Bearer Your_Token'

response = http.request(request)

puts response.body
```

ここで、SSLを有効にし、APIトークンで認証している。

## See Also (関連情報)
- `httparty` gem: [httparty](https://github.com/jnunemaker/httparty)
- `rest-client` gem: [rest-client](https://github.com/rest-client/rest-client)

これらはリクエスト送信に関するより詳細なリソースだ。
