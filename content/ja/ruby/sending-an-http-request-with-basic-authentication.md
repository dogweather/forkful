---
title:                "基本認証を使用してHTTPリクエストを送信する"
html_title:           "Ruby: 基本認証を使用してHTTPリクエストを送信する"
simple_title:         "基本認証を使用してHTTPリクエストを送信する"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Why 
なぜHTTPリクエストに基本認証を付けて送信するのか？

HTTPリクエストに基本認証を付けることで、セキュリティを強化し、アクセス制限をかけることができるためです。

## How To 
```Ruby 
require 'uri' 
require 'net/http'

uri = URI('https://example.com') 
req = Net::HTTP::Get.new(uri) 
req.basic_auth('username', 'password') 
res = Net::HTTP.start(uri.hostname, uri.port, use_ssl: uri.scheme == 'https') {|http| http.request(req)}

puts res.body
```
　
```
Example output:
200 OK
<p>Hello World</p>
```

## Deep Dive 
HTTPリクエストの`Authorization`ヘッダーには、ユーザー名とパスワードのBase64エンコードを含まれることにより、基本認証が行われます。Basic認証は、セキュリティ性の低い方式であるため、HTTPSを使用して通信を暗号化することが推奨されます。また、セキュリティを強化するためにユーザー名とパスワードを定期的に変更することも重要です。

## See Also 
参考リンク:

- [Rubyの公式ドキュメント](https://ruby-doc.org/stdlib-2.7.2/libdoc/net/http/rdoc/Net/HTTP.html)
- [Basic認証についての詳しい解説](https://developer.mozilla.org/ja/docs/Web/HTTP/Authentication#Basic_authentication_scheme) 
- [RubyでのHTTPリクエストの送信例](https://docs.ruby-lang.org/ja/latest/class/Net=3a=3aHTTP.html)