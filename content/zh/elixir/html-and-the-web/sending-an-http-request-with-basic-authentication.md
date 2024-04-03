---
date: 2024-01-20 18:01:14.105352-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C\uFF1A) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.359292-06:00'
model: gpt-4-1106-preview
summary: .
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
