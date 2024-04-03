---
date: 2024-01-20 18:00:21.459666-07:00
description: "How to: (\u3084\u308A\u65B9) Ruby\u3067\u306FNet::HTTP\u3092\u4F7F\u3063\
  \u3066HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\u308C\u308B\u3002\u3053\u3093\
  \u306A\u611F\u3058\uFF1A."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.850592-06:00'
model: gpt-4-1106-preview
summary: "Ruby\u3067\u306FNet::HTTP\u3092\u4F7F\u3063\u3066HTTP\u30EA\u30AF\u30A8\u30B9\
  \u30C8\u3092\u9001\u308C\u308B\u3002\u3053\u3093\u306A\u611F\u3058\uFF1A."
title: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306E\u9001\u4FE1"
weight: 44
---

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
