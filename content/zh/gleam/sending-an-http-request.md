---
title:                "发送http请求"
html_title:           "C#: 发送http请求"
simple_title:         "发送http请求"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 是什么 & 为什么？

HTTP请求就是客户端给服务器发送的一条信息，其中包括了你想请求的内容。程序员经常发送HTTP请求以获取一些在线信息或者连接到其他应用。

## 怎么做：

在Gleam中我们可以用`httpc:request`发送HTTP请求，下面是一个例子:

```gleam
let response =
  httpc.request(
    http.get("https://example.com")
  )

case response {
  Ok(#http.Response(body: body)) ->
    io.println(body)
  Error(e) ->
    io.println(e)
}
```
运行后，你会从"https://example.com"接收到的HTTP响应。

## 深入了解:

发送HTTP请求的方法早在90年代初期就开始出现，目的是为了能够让不同的设备和软件进行交互。
你也可以选择其他发送请求的库,例如`mochiweb`, 但是`httpc`库比较稳定，好用。
在实现上，这个请求信息经过互联网，到达服务器，服务器再返回响应信息。这个过程需要很精确的错误处理和重试机制。

## 另请参阅:

如果你对于Gleam编程语言有更多的兴趣，以下是一些有用的附加资源：
- [Gleam官方网站](https://gleam.run/)
- [Gleam GitHub 项目](https://github.com/gleam-lang/gleam)
- [Gleam语法详解](https://gleam.run/book/tour/introduction.html)