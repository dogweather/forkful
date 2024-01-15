---
title:                "发送http请求"
html_title:           "Gleam: 发送http请求"
simple_title:         "发送http请求"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 为什么要发送HTTP请求

发送HTTP请求是进行网络通信的一种方式，可以让我们的程序和其他服务器之间进行数据交互。这样可以让我们的程序获得其他服务器上的数据，或者将我们的数据发送到其他服务器上。

## 如何发送HTTP请求

发送HTTP请求可以通过Gleam的`httpc`模块来实现。下面是一个简单的例子，展示如何使用`http.request`函数来发送一个HTTP GET请求，并获取到返回的数据：

```Gleam
import gleam/httpc

let response =
  httpc.request("https://jsonplaceholder.typicode.com/todos/1", get) |> Httpc.send

let data = response.body |> list_to_bytes

// Output:
// `data == "{\"userId\": 1, \"id\": 1, \"title\":\"delectus aut autem\", \"completed\": false}"`
```
在上面的例子中，我们使用了`http.request`函数来发送一个GET请求给`https://jsonplaceholder.typicode.com/todos/1`这个URL。然后我们使用`Httpc.send`来发送请求并获取`response`对象。最后，我们通过`list_to_bytes`将数据转换成字节串。

## 深入探讨HTTP请求

除了上面示例中的GET请求外，我们还可以使用`httpc`模块来发送其他类型的请求，比如POST、DELETE、PUT等。同时，还可以设置请求头、传递参数、和处理返回的状态码等。关于`httpc`模块的更多信息，可以参考[官方文档](https://gleam.run/books/http/index.html)。

## 参考

- [Gleam官方文档-HTTP模块](https://gleam.run/books/http/index.html)
- [HTTP请求详解](https://www.runoob.com/http/http-tutorial.html)
- [HTTP请求的工作原理](https://developer.mozilla.org/zh-CN/docs/Web/HTTP/Overview)