---
title:                "使用基本身份验证发送http请求"
html_title:           "Ruby: 使用基本身份验证发送http请求"
simple_title:         "使用基本身份验证发送http请求"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 为什么

在编写网络应用程序时，可能需要使用HTTP请求来与其他系统通信。使用基本认证是一种常见的安全措施，可以确保只有经过授权的用户才能访问系统。

## 如何

```Ruby
require 'uri'
require 'net/http'

uri = URI('https://example.com/api/posts')
req = Net::HTTP::Post.new(uri)
req.basic_auth('username', 'password')

res = Net::HTTP.start(uri.hostname, uri.port, use_ssl: uri.scheme == 'https') do |http|
  http.request(req)
end

puts res.code     # 200
puts res.message  # OK
puts res.body     # response body from server
```

## 深入了解

要发送带有基本认证的HTTP请求，需要使用Net::HTTP标准库。在发送请求之前，必须创建一个URI对象，并使用基本信息（用户名和密码）创建一个Net::HTTP::Post对象。使用start方法来实际发起请求，并通过调用request方法来发送请求。最后，可以从服务器返回的res对象中获取响应的代码、消息和正文内容。

## 参考资料

- 官方Ruby文档：https://ruby-doc.org/stdlib/libdoc/net/http/rdoc/Net/HTTP.html
- HTTP基本认证介绍：https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme