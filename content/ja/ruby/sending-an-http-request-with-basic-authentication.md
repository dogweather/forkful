---
title:                "基本認証を使用してhttpリクエストを送信する"
html_title:           "C#: 基本認証を使用してhttpリクエストを送信する"
simple_title:         "基本認証を使用してhttpリクエストを送信する"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 何と何のために？
HTTPリクエストの基本認証とは、ユーザー名とパスワードを提供することでサーバーとの通信を認証する方法です。これは、特定のリソースへの許可付きアクセスや、安全な情報交換を行うためにプログラマーによって行われます。

## どのように：
RubyにはNet::HTTPライブラリが用意されており、これを使用してHTTPリクエストを送信しましょう。以下に簡単なコード例を示します：

```ruby
require 'net/http'
require 'uri'

uri = URI.parse("http://example.com/")

Net::HTTP.start(uri.host, uri.port) do |http|
  request = Net::HTTP::Get.new(uri.request_uri)
  request.basic_auth('username', 'password')
  response = http.request(request)
  puts response.body
end
```

この例ではHTTPのGETリクエストを行い、ユーザー名とパスワードを認証として設定しています。

## ディープダイブ
基本認証はHTTP/1.0の時代から存在しており、シンプルさと理解しやすさから幅広く利用されています。しかし、これは情報を暗号化せずに送信するため安全性に欠け、HTTPSなど他の認証方法への移行が推奨されています。

また、上記のコードは最も基本的な形で、エラーハンドリングやリダイレクトの取扱いなどは未実装のままです。実際には、これらの機能を追加したり、サードパーティ製のライブラリ（Rest-Client等）を利用したりすることも多いです。

## 参考文献
* [RubyのNet::HTTPについて](https://docs.ruby-lang.org/ja/latest/library/net=2fhttp.html)
* [HTTP Basic Authentication](https://www.ietf.org/rfc/rfc2617.txt)