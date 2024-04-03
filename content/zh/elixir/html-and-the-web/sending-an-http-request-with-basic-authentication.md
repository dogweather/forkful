---
date: 2024-01-20 18:01:14.105352-07:00
description: "\u53D1\u9001\u5E26\u6709\u57FA\u672C\u8BA4\u8BC1\u7684HTTP\u8BF7\u6C42\
  \u662F\u901A\u8FC7\u7F51\u7EDC\u8BBF\u95EE\u53D7\u4FDD\u62A4\u8D44\u6E90\u7684\u65B9\
  \u5F0F\uFF0C\u5B83\u5C06\u7528\u6237\u540D\u548C\u5BC6\u7801\u7ECF\u8FC7\u7F16\u7801\
  \u9644\u52A0\u5728\u8BF7\u6C42\u5934\u4E2D\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\
  \u662F\u4E3A\u4E86\u786E\u4FDD\u53EA\u6709\u62E5\u6709\u6B63\u786E\u51ED\u8BC1\u7684\
  \u7528\u6237\u53EF\u4EE5\u8BBF\u95EE\u7279\u5B9A\u6570\u636E\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.359292-06:00'
model: gpt-4-1106-preview
summary: "\u53D1\u9001\u5E26\u6709\u57FA\u672C\u8BA4\u8BC1\u7684HTTP\u8BF7\u6C42\u662F\
  \u901A\u8FC7\u7F51\u7EDC\u8BBF\u95EE\u53D7\u4FDD\u62A4\u8D44\u6E90\u7684\u65B9\u5F0F\
  \uFF0C\u5B83\u5C06\u7528\u6237\u540D\u548C\u5BC6\u7801\u7ECF\u8FC7\u7F16\u7801\u9644\
  \u52A0\u5728\u8BF7\u6C42\u5934\u4E2D\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\
  \u4E3A\u4E86\u786E\u4FDD\u53EA\u6709\u62E5\u6709\u6B63\u786E\u51ED\u8BC1\u7684\u7528\
  \u6237\u53EF\u4EE5\u8BBF\u95EE\u7279\u5B9A\u6570\u636E\u3002."
title: "\u4F7F\u7528\u57FA\u672C\u8BA4\u8BC1\u53D1\u9001 HTTP \u8BF7\u6C42"
weight: 45
---

## How to: (如何操作：)
```Elixir
# 使用HTTPoison库发送带有基本认证的GET请求
HTTPoison.start()

auth = {"user", "pass"} |> Base.encode64()
headers = [{"Authorization", "Basic #{auth}"}]
url = "http://example.com/protected/resource"

{:ok, response} = HTTPoison.get(url, headers)
IO.inspect(response.status_code) # 输出状态码
IO.puts(response.body)           # 输出响应主体
```
输出样例：
```
200
"This is the protected resource content."
```

## Deep Dive (深入探索)
发送HTTP请求时基本认证是一种验证HTTP用户的方法，出现在HTTP/1.0协议中。替代方案包括Digest访问认证、OAuth和JWT等。对于基本认证，安全性较低，因为如果不使用HTTPS，编码的用户名和密码可被拦截。Elixir中，除了HTTPoison，还可以使用其他库，例如Tesla或HTTPotion。为保证安全，生产环境下发送敏感信息需通过HTTPS。

## See Also (另请参阅)
- HTTPoison documentation: https://hexdocs.pm/httpoison
- Elixir's HTTP clients comparison: https://elixirforum.com/t/http-client-libraries-in-elixir/18987
- Base64 encoding in Elixir: https://hexdocs.pm/elixir/Base.html
- Basic authentication in HTTP: https://tools.ietf.org/html/rfc7617
