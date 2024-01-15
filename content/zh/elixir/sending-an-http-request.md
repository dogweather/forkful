---
title:                "发送一个http请求"
html_title:           "Elixir: 发送一个http请求"
simple_title:         "发送一个http请求"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 为什么

HTTP请求是现代网络通信中不可或缺的一部分。通过发送HTTP请求，我们可以获取信息，进行数据交换，甚至实现网页浏览。在Elixir编程中，使用HTTP请求可以轻松地与其他服务器或服务进行通信，为我们的应用程序带来更多功能。

## 如何使用

让我们来看一个简单的例子如何发送HTTP请求：

```elixir
url = "https://example.com/api/users/1"
response = HTTPoison.get(url)

IO.puts response.body
# 输出："{ "id": 1, "name": "John Doe" }"
```

上面的代码示例使用了第三方库HTTPoison来发送GET请求，并从服务器获取用户信息。首先，我们将请求的URL保存在变量中，然后使用HTTPoison库的`get/2`函数来发送请求。最后，我们通过输出响应的body属性来查看服务器返回的信息。

除了GET请求外，我们还可以使用HTTPoison来发送POST、PUT、PATCH、DELETE等常见的HTTP请求，以及设置请求头和参数。通过查阅官方文档，我们可以发现更多丰富的功能和用法。

## 深入探讨

Elixir的标准库中也提供了`HTTP`模块，可以用来发送HTTP请求。和HTTPoison相比，`HTTP`模块更加灵活，可以自定义更多的请求参数。让我们来看一个使用`HTTP.get/3`函数的例子：

```elixir
url = "https://example.com/api/users"
response = HTTP.get(url, headers: [{"Accept", "application/json"}])

IO.puts response.body
# 输出："[{ "id": 1, "name": "John Doe" }, { "id": 2, "name": "Jane Smith" }]"
```

在上面的例子中，我们通过在请求中设置header头来指定需要返回json格式的数据。这个功能可以在处理不同格式数据时非常有用。

另外，使用`HTTP.get!/3`函数可以在请求出错时抛出异常，从而更好地处理错误情况。

## 参考链接

- [HTTPoison官方文档](https://github.com/edgurgel/httpoison)
- [Elixir官方文档 - HTTP模块](https://hexdocs.pm/elixir/HTTP.html)