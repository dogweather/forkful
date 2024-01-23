---
title:                "HTTPリクエストの送信"
date:                  2024-01-20T18:00:21.459666-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTPリクエストの送信"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/sending-an-http-request.md"
---

{{< edit_this_page >}}

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
