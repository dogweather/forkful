---
title:                "Ruby: 基本認証を使用してhttpリクエストを送信する"
simple_title:         "基本認証を使用してhttpリクエストを送信する"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## なぜ
誰かが基本認証を使用してHTTPリクエストを送信する理由を理解するには、セキュリティが重要であることがわかります。

## 方法
````Ruby
require 'net/http'
require 'json'

url = URI('https://example.com/api/user')
http = Net::HTTP.new(url.host, url.port)
http.use_ssl = true # この行は、https接続でのみ必要です
request = Net::HTTP::Get.new(url)
request['Authorization'] = 'Basic dXNlcm5hbWU6cGFzc3dvcmQ=' # ユーザー名:パスワードの Base64 エンコード文字列
response = http.request(request)
puts JSON.pretty_generate(JSON.parse(response.body))
````
実行結果:
```
{
  "user": {
    "name": "John",
    "age": 28
  }
}
```

## ディープダイブ
基本認証は、セキュリティのためにデフォルトで有効になっていたが、現在は安全とは見なされていません。そのため、HTTPS接続によるエンドポイントとの通信には、より安全な認証方法を選択することをお勧めします。また、ユーザー名とパスワードを明示的に入力することではなく、環境変数などのセキュリティ設定からデータを取得する方法もあります。

## 参考リンク
- [Ruby Net::HTTPガイド](https://docs.ruby-lang.org/ja/2.5.0/Net/HTTP.html)
- [Base64エンコードについてのRubyドキュメント](https://docs.ruby-lang.org/ja/2.5.0/library/base64.html)
- [HTTPクライアントの自動認証の詳細（英語）](https://aaronparecki.com/2017/01/23/9/http-client-cert)
- [Rubyにおける環境変数の扱い方についてのガイド](https://qiita.com/Alex_mhtcode/items/cad4c15c83208e7fdffd)