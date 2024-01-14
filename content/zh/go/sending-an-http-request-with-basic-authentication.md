---
title:                "Go: 发送带有基本身份验证的http请求"
simple_title:         "发送带有基本身份验证的http请求"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 为什么

从我们的每日生活到互联网的世界，我们都在与不同的网站和应用程序进行交互。这些网站和应用程序需要身份验证来保护用户的信息和数据。基本身份验证是一种常用的身份验证方法，通过发送HTTP请求来验证用户身份。如果您想要学习如何通过Go编程来发送带有基本身份验证的HTTP请求，那么这篇文章就是为您准备的。

## 如何做

要发送带有基本身份验证的HTTP请求，我们需要使用Go语言的"net/http"包。首先，我们需要导入这个包，并创建一个"Request"结构体。然后，我们需要指定在发送请求时要使用的URL，如下所示：

```Go
import "net/http"
req, err := http.NewRequest("GET", "https://example.com", nil)
```

接下来，我们需要使用"SetBasicAuth"方法来设置基本身份验证，并将用户名和密码作为参数传递进去。最后，我们可以使用"go-client"来执行我们的请求，并打印响应。

```Go
req.SetBasicAuth("username", "password")
client := &http.Client{}
resp, err := client.Do(req)
fmt.Println(resp)
```

当我们运行这段代码时，它将发送一个带有基本身份验证的HTTP请求，并返回响应。输出可能如下所示：

```
&{200 OK 200 HTTP/2.0 2 0 map[Content-Type:[text/html; charset=utf-8] Date:[Wed, 05 Aug 2020 12:00:00 GMT] Server:[nginx]] <nil> 0 []
false false  [] 0xc0000e8000}
```

## 深入探索

现在让我们来深入了解一下发送带有基本身份验证的HTTP请求。它的基本原理是，当我们使用"go-client"来执行请求时，它会收集User-Agent、主机、授权等信息，然后通过"SetBasicAuth"方法将其添加到请求头中。当服务器收到请求时，它将检查这些信息，并基于它们来验证用户身份。

在这篇文章中，我们随机生成了一个用于演示的URL和用户名/密码。在您的实际项目中，您需要使用真实的URL和安全的用户名/密码来发送请求。

## 参考链接

如果您想深入了解如何使用Go编程来发送带有基本身份验证的HTTP请求，请参考以下链接：

- https://golang.org/pkg/net/http/
- https://blog.golang.org/error-handling-and-go
- https://github.com/golang/go/wiki/Errors
- https://gobyexample.com/http-clients
- https://www.digitalocean.com/community/tutorials/building-and-testing-a-rest-api-in-go-with-gorilla-mux-and-postgresql