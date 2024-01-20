---
title:                "发送http请求"
html_title:           "C#: 发送http请求"
simple_title:         "发送http请求"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 什么和为什么?

发送HTTP请求是一种让计算机向服务器发送特定请求并获取响应的方式。程序员需要这样做以从远程服务器获取数据，或者影响服务器的状态。

## 如何操作:

在 Ruby 中发送 HTTP 请求很简单。假设我们想要从 "httpbin.org" 获取 IP 地址, Ruby 代码可能如下:

```Ruby
require 'net/http'
require 'uri'

uri = URI.parse("http://httpbin.org/ip")
response = Net::HTTP.get_response(uri)

puts response.body
```

运行代码之后, 返回值将是类似下面这样的字符串:

```Ruby
{
  "origin": "123.456.789.10"
}
```
## 深入研究:

发送HTTP请求的方法有很多种。我们选用了Ruby的 `Net::HTTP`库，主要因为它在Ruby标准库中并且易于上手。然而，有其他的HTTP客户端库例如 Rest-Client 和 Faraday，它们提供更高级的功能，如中间件支持和异步请求。

发送HTTP请求通常涉及创建一个有特定请求方法（如 GET、POST 或 DELETE）的请求对象，并传递给远程服务器。请求的发出和相应的接收都是用TCP/IP协议完成的。

## 参考文献:

- Ruby 文档中的 `Net::HTTP`: https://ruby-doc.org/stdlib-3.1.0/libdoc/net/http/rdoc/Net/HTTP.html
- 更先进的 HTTP 客户端库, 如 Rest-Client: https://github.com/rest-client/rest-client
- 关于 TCP/IP 的详细信息: https://www.ibm.com/docs/en/zos/2.3.0?topic=protocols-tcpip