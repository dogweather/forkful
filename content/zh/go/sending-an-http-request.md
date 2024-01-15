---
title:                "发送一个 http 请求"
html_title:           "Go: 发送一个 http 请求"
simple_title:         "发送一个 http 请求"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 为什么

想象一下，你在使用网上购物网站，点击购买按钮后发现订单提交出现了问题。这时，网站可能会提示你发送一个HTTP请求来解决这个问题。

## 如何进行HTTP请求

发送HTTP请求的最简单方式是使用Go标准库中的`net/http`包。我们首先需要使用`net/http`包中的`Get`函数来建立一个新的HTTP请求，然后指定请求的URL。接着，我们可以通过调用`Response`结构体的`Body`字段来获取响应的主体内容。最后，我们使用`io.Copy`函数将响应的主体内容写入标准输出。

```Go
resp, err := http.Get("https://example.com")
if err != nil {
  // 处理错误
}
defer resp.Body.Close()

_, err = io.Copy(os.Stdout, resp.Body)
if err != nil {
  // 处理错误
}
```

通过运行上述代码，你将能够发送一个HTTP请求并获取响应的内容。如果你想要发送不同类型的HTTP请求，例如POST或PUT请求，可以使用`http.NewRequest`函数来创建一个新的请求并指定请求方法，如下所示：

```Go
req, err := http.NewRequest("POST", "https://example.com", body)
// 其中，body是一个io.Reader类型的数据，可以是字符串、字节数组等
```

## 深入探讨HTTP请求

HTTP请求是客户端通过网络与服务器进行通信的一种方式。请求中包含了请求的方法、URL、HTTP版本以及可选的请求头和请求体。在Go中，可以通过`http.Request`结构体来表示一个HTTP请求，该结构体包含了上述所有信息。

另外，HTTP请求也可以通过访问地址和查询参数来包含更多的信息。例如，在请求`https://example.com?name=John&age=25`时，可以通过访问`req.URL.Query().Get("name")`来获取`John`作为查询参数的值。

## 参考链接

- [Go标准库`net/http`文档](https://golang.org/pkg/net/http/)
- [Go标准库`io`文档](https://golang.org/pkg/io/)
- [HTTP请求详解](https://developer.mozilla.org/zh-CN/docs/Web/HTTP/Overview)