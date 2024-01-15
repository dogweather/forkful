---
title:                "「httpリクエストを送信する」"
html_title:           "Ruby: 「httpリクエストを送信する」"
simple_title:         "「httpリクエストを送信する」"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/sending-an-http-request.md"
---

{{< edit_this_page >}}

## なぜ

HTTPリクエストを送信することのメリットは、インターネットを介して情報を取得したり、データを送信したりすることができることです。これは、Webアプリケーションを開発する際に非常に重要な機能です。

## 方法

```Ruby
require 'net/http'
url = URI("https://www.example.com")
http = Net::HTTP.new(url.host, url.port)
http.use_ssl = true
request = Net::HTTP::Get.new(url)
response = http.request(request)
puts response.body
```

このコードは、RubyのNet::HTTPライブラリを使用して、https://www.example.comにGETリクエストを送信しています。リクエストに対するレスポンスは、responseという変数に保存され、そのbodyメソッドを使用して結果を取得します。

## 深堀り

HTTPリクエストには、GET、POST、PUT、DELETEなどのメソッドがあります。また、リクエストに必要なパラメーターやヘッダーを指定することもできます。さらに、レスポンスのステータスコードやヘッダーの情報も取得することができます。詳細な情報は、RubyのNet::HTTPドキュメントを参照してください。

## 他に見る

- https://docs.ruby-lang.org/en/2.7.0/Net/HTTP.html
- https://www.rubyguides.com/2018/08/net-http-ruby/
- https://www.tutorialspoint.com/ruby-on-rails/rails-and-http-requests.htm