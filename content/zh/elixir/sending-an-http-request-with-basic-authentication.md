---
title:                "使用基本认证发送 HTTP 请求"
date:                  2024-01-20T18:01:14.105352-07:00
model:                 gpt-4-1106-preview
simple_title:         "使用基本认证发送 HTTP 请求"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why? (什么 & 为什么？)
发送带有基本认证的HTTP请求是通过网络访问受保护资源的方式，它将用户名和密码经过编码附加在请求头中。程序员这样做是为了确保只有拥有正确凭证的用户可以访问特定数据。

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