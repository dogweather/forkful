---
aliases:
- /zh/ruby/sending-an-http-request-with-basic-authentication/
date: 2024-01-20 18:02:33.023213-07:00
description: "\u53D1\u9001\u5E26\u6709\u57FA\u672C\u8BA4\u8BC1\u7684HTTP\u8BF7\u6C42\
  \uFF0C\u5C31\u50CF\u662F\u7ED9\u7F51\u7EDC\u8BF7\u6C42\u8D34\u4E0A\u8EAB\u4EFD\u6807\
  \u7B7E\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u4E3B\u8981\u4E3A\u4E86\u5B89\u5168\
  \u5730\u8BBF\u95EE\u53D7\u4FDD\u62A4\u7684\u7F51\u7EDC\u8D44\u6E90\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:59.600726
model: gpt-4-1106-preview
summary: "\u53D1\u9001\u5E26\u6709\u57FA\u672C\u8BA4\u8BC1\u7684HTTP\u8BF7\u6C42\uFF0C\
  \u5C31\u50CF\u662F\u7ED9\u7F51\u7EDC\u8BF7\u6C42\u8D34\u4E0A\u8EAB\u4EFD\u6807\u7B7E\
  \u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u4E3B\u8981\u4E3A\u4E86\u5B89\u5168\u5730\
  \u8BBF\u95EE\u53D7\u4FDD\u62A4\u7684\u7F51\u7EDC\u8D44\u6E90\u3002"
title: "\u4F7F\u7528\u57FA\u672C\u8BA4\u8BC1\u53D1\u9001 HTTP \u8BF7\u6C42"
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
