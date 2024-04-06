---
date: 2024-01-20 18:01:14.105352-07:00
description: ''
isCJKLanguage: true
lastmod: '2024-04-05T22:51:00.579515-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u64CD\u4F5C\uFF1A) \u53D1\u9001HTTP\u8BF7\u6C42\u65F6\u57FA\
  \u672C\u8BA4\u8BC1\u662F\u4E00\u79CD\u9A8C\u8BC1HTTP\u7528\u6237\u7684\u65B9\u6CD5\
  \uFF0C\u51FA\u73B0\u5728HTTP/1.0\u534F\u8BAE\u4E2D\u3002\u66FF\u4EE3\u65B9\u6848\
  \u5305\u62ECDigest\u8BBF\u95EE\u8BA4\u8BC1\u3001OAuth\u548CJWT\u7B49\u3002\u5BF9\
  \u4E8E\u57FA\u672C\u8BA4\u8BC1\uFF0C\u5B89\u5168\u6027\u8F83\u4F4E\uFF0C\u56E0\u4E3A\
  \u5982\u679C\u4E0D\u4F7F\u7528HTTPS\uFF0C\u7F16\u7801\u7684\u7528\u6237\u540D\u548C\
  \u5BC6\u7801\u53EF\u88AB\u62E6\u622A\u3002Elixir\u4E2D\uFF0C\u9664\u4E86HTTPoison\uFF0C\
  \u8FD8\u53EF\u4EE5\u4F7F\u7528\u5176\u4ED6\u5E93\uFF0C\u4F8B\u5982Tesla\u6216HTTPotion\u3002\
  \u4E3A\u4FDD\u8BC1\u5B89\u5168\uFF0C\u751F\u4EA7\u73AF\u5883\u4E0B\u53D1\u9001\u654F\
  \u611F\u4FE1\u606F\u9700\u901A\u8FC7HTTPS\u3002"
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
