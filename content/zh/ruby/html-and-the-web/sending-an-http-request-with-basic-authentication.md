---
date: 2024-01-20 18:02:33.023213-07:00
description: "How to: HTTP\u57FA\u672C\u8BA4\u8BC1\u7684\u5386\u53F2\u53EF\u4EE5\u8FFD\
  \u6EAF\u5230\u65E9\u671F\u7684web\u3002\u5B83\u662F\u4E00\u79CD\u7B80\u5355\u7684\
  \u8BA4\u8BC1\u673A\u5236\uFF0C\u4F46\u5E76\u4E0D\u662F\u6700\u5B89\u5168\u7684\u2014\
  \u5B83\u5C06\u7528\u6237\u540D\u548C\u5BC6\u7801\u4EE5base64\u7F16\u7801\u7684\u5F62\
  \u5F0F\u53D1\u9001\u3002\u6BD4\u8D77\u5176\u4ED6\u8BA4\u8BC1\u65B9\u5F0F\uFF0C\u6BD4\
  \u5982OAuth\u6216JWT\uFF0C\u57FA\u672C\u8BA4\u8BC1\u7684\u90E8\u7F72\u548C\u4F7F\
  \u7528\u90FD\u7B80\u5355\u5F97\u591A\u3002\u7136\u800C\uFF0C\u5B83\u901A\u5E38\u5E94\
  \u8BE5\u53EA\u5728HTTPS\u8FDE\u63A5\u4E0B\u4F7F\u7528\uFF0C\u4EE5\u907F\u514D\u51ED\
  \u636E\u88AB\u7A83\u3002\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:01.574059-06:00'
model: gpt-4-1106-preview
summary: "HTTP\u57FA\u672C\u8BA4\u8BC1\u7684\u5386\u53F2\u53EF\u4EE5\u8FFD\u6EAF\u5230\
  \u65E9\u671F\u7684web\u3002\u5B83\u662F\u4E00\u79CD\u7B80\u5355\u7684\u8BA4\u8BC1\
  \u673A\u5236\uFF0C\u4F46\u5E76\u4E0D\u662F\u6700\u5B89\u5168\u7684\u2014\u5B83\u5C06\
  \u7528\u6237\u540D\u548C\u5BC6\u7801\u4EE5base64\u7F16\u7801\u7684\u5F62\u5F0F\u53D1\
  \u9001\u3002\u6BD4\u8D77\u5176\u4ED6\u8BA4\u8BC1\u65B9\u5F0F\uFF0C\u6BD4\u5982OAuth\u6216\
  JWT\uFF0C\u57FA\u672C\u8BA4\u8BC1\u7684\u90E8\u7F72\u548C\u4F7F\u7528\u90FD\u7B80\
  \u5355\u5F97\u591A\u3002\u7136\u800C\uFF0C\u5B83\u901A\u5E38\u5E94\u8BE5\u53EA\u5728\
  HTTPS\u8FDE\u63A5\u4E0B\u4F7F\u7528\uFF0C\u4EE5\u907F\u514D\u51ED\u636E\u88AB\u7A83\
  \u3002"
title: "\u4F7F\u7528\u57FA\u672C\u8BA4\u8BC1\u53D1\u9001 HTTP \u8BF7\u6C42"
weight: 45
---

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
