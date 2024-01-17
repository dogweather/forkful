---
title:                "基本認証でhttpリクエストを送信する"
html_title:           "Ruby: 基本認証でhttpリクエストを送信する"
simple_title:         "基本認証でhttpリクエストを送信する"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 何？なぜ？

基本認証を使用してHTTPリクエストを送信するとは何か、そしてなぜプログラマーがこれを行うのかを説明します。

基本認証を使用してHTTPリクエストを送信するとは、サーバーに直接アクセスしてユーザー名とパスワードを送信することを意味します。プログラマーは、クライアントとサーバー間の通信を確立するためにこの方法を使用します。

## 方法：

以下のコードブロックを使用して、基本認証を使用してHTTPリクエストを送信する方法を示します。

```Ruby 
require 'net/http'
require 'uri'

uri = URI.parse("https://example.com")
req = Net::HTTP::Get.new(uri.request_uri)

req.basic_auth 'username', 'password'

response = Net::HTTP.start(uri.hostname, uri.port, :use_ssl => uri.scheme == 'https') do |http|
  http.request(req)
end

puts response.body
```

上記の例では、`example.com` というサイトにユーザー名とパスワードを使用してHTTPリクエストを送信しています。レスポンスから返されたデータを `puts` を使用してコンソールに出力します。

## ディープダイブ：

基本認証は、1999年に最初にRFCによって定義された古い認証プロトコルです。しかし、まだ多くのWebサイトで使用されています。代替手段としては、よりセキュアな認証方式であるOAuthやOpenIDがあります。

上記のコードはRubyの基本的な例ですが、実際にはさまざまなライブラリやフレームワークを使用して実装することができます。また、基本認証はユーザー名とパスワードを平文で送信するため、セキュリティ上のリスクがあります。そのため、HTTPSを使用することをお勧めします。

## 関連リンク：

- [RFC2617 - HTTP Basic and Digest Access Authentication](https://tools.ietf.org/html/rfc2617)
- [OAuth公式サイト](https://oauth.net/)
- [OpenID公式サイト](https://openid.net/)
- [Ruby Net::HTTPドキュメント](https://ruby-doc.org/stdlib-2.6.3/libdoc/net/http/rdoc/Net/HTTP.html)