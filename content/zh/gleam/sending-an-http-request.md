---
title:                "Gleam: 将http请求发送"
simple_title:         "将http请求发送"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

# 为什么
任何程序员都知道，在网络编程中，发送HTTP请求是必不可少的。无论是获取网页内容、与服务器进行通信，还是调用API接口，都需要发送HTTP请求。Gleam提供了一种简单而高效的方式来处理HTTP请求，使得网络编程变得更加容易和流畅。

# 如何
Gleam中，发送HTTP请求只需要使用一个简单的函数：`httpc.request()`。下面是一个示例代码，展示了如何使用这个函数来发送一个HTTP GET请求，并获取服务器返回的数据。

```Gleam
let url = "https://jsonplaceholder.typicode.com/posts/1"
let headers = [("Content-Type", "application/json")]
let response = httpc.request("GET", url, headers, "")
let body = response.body

IO.println("Response Body:")
IO.println(body)
```

运行上述代码，你将会得到以下输出：

```Gleam
Response Body:
{"userId": 1, "id": 1, "title": "sunt aut facere repellat..."
```

如你所见，我们成功发送了一个HTTP GET请求，并从服务器获取了响应数据。这个函数还可以接受其他参数，比如请求体、响应的状态码、响应头部等等，让你更加灵活地处理HTTP请求。

# 深入探讨
Gleam中的`httpc`模块其实是通过调用Erlang标准库中的`:httpc`模块来实现的。这意味着我们可以使用Erlang中的一些函数来更加深入地探索HTTP请求的处理过程。比如，我们可以使用`httpc:request()`函数来直接发送HTTP请求，而不需要通过Gleam的封装。

除此之外，Gleam也提供了`httpc.decoding`模块，用于帮助解析和处理服务器响应的数据。你可以使用`httpc.decoding.response()`函数来解析响应的JSON数据，或者`httpc.decoding.html()`函数来解析响应的HTML数据。

# 查看也许
- [Gleam官方文档](https://gleam.run/documentation/#httpc)
- [Erlang标准库文档：httpc](http://erlang.org/doc/man/httpc.html)
- [以Gleam为中心的HTTP请求教程](https://scylladb.com/blog/http-requests-in-gleam-a-tutorial/)