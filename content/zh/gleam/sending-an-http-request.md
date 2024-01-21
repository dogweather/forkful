---
title:                "发出 HTTP 请求"
date:                  2024-01-20T17:59:47.748206-07:00
model:                 gpt-4-1106-preview
simple_title:         "发出 HTTP 请求"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)

发送HTTP请求即是让程序通过互联网向服务端传达信息。程序员这么做主要是为了获取数据、提交表单，或者与RESTful APIs交互。

## How to: (怎么做：)

Gleam中发送HTTP请求可以使用`gleam/http`库。下面是一个简单例子：

```gleam
import gleam/http
import gleam/should

pub fn send_request() {
  let response = http.get("https://api.example.com/data")
  should.equal(response.status, 200)
  should.equal(response.body, "Here's your data!")
}
```

运行这个函数，如果一切顺利，它会从`https://api.example.com/data` 获取数据并验证响应。

## Deep Dive (深入了解)

HTTP请求的概念可以追溯到早期的Web，它是基于客户端与服务器间通信的基石。Gleam的`http`库提供了一种类型安全和友好的方式来构建这些请求。除了Gleam自己的`http`库，其他语言例如Python有`requests`、Node.js有`axios`，都是流行的选择。使用Gleam的优势在于它结合了静态类型系统和Erlang虚拟机的高并发特性，让你构建健壮且可扩展的应用程序。

## See Also (参考链接)

- Gleam HTTP library documentation: [https://hexdocs.pm/gleam_http/](https://hexdocs.pm/gleam_http/)
- Erlang/OTP: [https://www.erlang.org/](https://www.erlang.org/)
- RESTful API design best practices: [https://restfulapi.net/](https://restfulapi.net/)