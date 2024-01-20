---
title:                "发送http请求"
html_title:           "C#: 发送http请求"
simple_title:         "发送http请求"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/lua/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
HTTP请求是客户端向服务器发送的特定类型的请求。程序员使用它来获取服务器上的数据，理解其状态，或向服务器发送数据。

## 如何实现：
在Lua中，我们可以使用`socket.http`库来发送HTTP请求。以下是一个简单的例子：
```Lua
local http = require("socket.http")

-- 请求URL
local url = "http://www.example.com"

-- 发送HTTP请求
local response, status, headers = http.request(url)

-- 打印响应
if status == 200 then
    print(response)
end
```
这段代码会发送一个GET请求到`http://www.example.com`，并打印出返回的响应。如果请求成功，HTTP状态码将被设置为200。

## 深入学习：
发送HTTP请求是网络编程的基础。早前的HTTP/0.9版本在1989年出现，但在当今的互联网应用中，最常见的是HTTP/1.1和HTTP/2。

除了使用`socket.http`库外，你还可以使用较新的`lua-http`库，它提供了更为现代化和全面的功能。

HTTP请求采用基于文本的协议，使用了一种特殊的格式化结构。这让我们可以通过阅读和理解这些消息，从而更好地理解它是如何操作的。

## 参考链接：
1. [Lua socket.http库文档](http://w3.impa.br/~diego/software/luasocket/http.html)
2. [Lua-http库文档](https://github.com/daurnimator/lua-http)
3. [HTTP历史和相关信息](https://developer.mozilla.org/zh-CN/docs/Web/HTTP/Overview)