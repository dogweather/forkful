---
title:                "使用基本认证发送http请求"
html_title:           "Haskell: 使用基本认证发送http请求"
simple_title:         "使用基本认证发送http请求"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 为什么

发送HTTP请求是一种常见的网络交互方式，而使用基本身份验证可以增加对敏感信息的保护。通过这种方式，服务器可以验证用户的身份，并且只有经过身份验证的用户才能访问特定的资源。

## 怎样做

使用Haskell发送带有基本身份验证的HTTP请求非常简单。首先，我们需要导入"Network.HTTP"模块，并定义我们要访问的URL：

```Haskell
import Network.HTTP

url = "https://example.com"
```

接下来，我们可以使用"simpleHTTP"函数来发送我们的HTTP请求。该函数接受一个"Request"类型的参数，并返回一个"IO (Either IOException (Response ByteString))"类型的值。请求的参数由"getDefaultRequest"函数生成，我们需要将URL作为参数传递给它：

```Haskell
response <- simpleHTTP (getDefaultRequest url)
```

我们还可以通过"setRequestMethod"函数设置请求的方法，以及通过"setRequestBasicAuth"函数添加基本身份验证。这两个函数都接受"Request"类型的参数，并返回修改后的请求。例如，如果我们想使用GET方法和用户名和密码进行身份验证，我们可以这样做：

```Haskell
request = setRequestMethod (getDefaultRequest url) "GET"
authRequest = setRequestBasicAuth request "username" "password"
```

最后，我们使用"getResponseBody"函数来从响应中获取正文，并将其打印出来：

```Haskell
let body = getResponseBody response
print body
```

完整的代码示例如下：

```Haskell
import Network.HTTP

main = do
    let url = "https://example.com"
    response <- simpleHTTP (setRequestBasicAuth (setRequestMethod (getDefaultRequest url) "GET") "username" "password")
    let body = getResponseBody response
    print body
```

运行该程序，我们可以看到响应的正文被打印出来。

## 深入探讨

除了使用基本身份验证外，我们还可以通过其他方式来发送HTTP请求，例如使用OAuth 2.0认证来授权访问。此外，在发送HTTP请求时，我们还可以指定请求的头部信息、内容类型以及设置超时时间。有关更多详细信息，请参阅Haskell的网络编程文档。

## 参考资料

- [Haskell网络编程文档](https://hackage.haskell.org/package/network-3.1.1.1/docs/Network-HTTP.html)
- [OAuth 2.0官方网站](https://oauth.net/2/)