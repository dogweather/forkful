---
title:                "使用基本认证发送 HTTP 请求"
aliases:
- /zh/ruby/sending-an-http-request-with-basic-authentication/
date:                  2024-01-20T18:02:33.023213-07:00
model:                 gpt-4-1106-preview
simple_title:         "使用基本认证发送 HTTP 请求"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why?
发送带有基本认证的HTTP请求，就像是给网络请求贴上身份标签。程序员这么做主要为了安全地访问受保护的网络资源。

## How to:
Ruby发送HTTP请求很简单。这里有个示例：

```Ruby
require 'net/http'
require 'uri'

uri = URI.parse("http://example.com/secrets")
request = Net::HTTP::Get.new(uri)
request.basic_auth("user", "password")

response = Net::HTTP.start(uri.hostname, uri.port) do |http|
  http.request(request)
end

puts response.body
```

如果一切顺利，你将看到服务器的响应内容。

## Deep Dive
HTTP基本认证的历史可以追溯到早期的web。它是一种简单的认证机制，但并不是最安全的—它将用户名和密码以base64编码的形式发送。比起其他认证方式，比如OAuth或JWT，基本认证的部署和使用都简单得多。然而，它通常应该只在HTTPS连接下使用，以避免凭据被窃。

实施基本认证时，需要注意以下几点：
- 保持你的凭据安全，最好不要硬编码在代码中。
- 理解HTTP请求如何在你的应用和服务器之间传输，特别是要确保使用SSL/TLS。
- 选用更高级的认证机制以提供更强的安全性。

## See Also
- 关于HTTP基本认证的进一步信息： [HTTP authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- 如何在Ruby on Rails中使用HTTP基本认证： [ActionController::HttpAuthentication::Basic](https://api.rubyonrails.org/classes/ActionController/HttpAuthentication/Basic.html)
