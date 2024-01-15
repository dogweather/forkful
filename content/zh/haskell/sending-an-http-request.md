---
title:                "发送Http请求。"
html_title:           "Haskell: 发送Http请求。"
simple_title:         "发送Http请求。"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 为什么

发送HTTP请求是在编写程序中经常会遇到的一项任务。当我们需要与其他网络资源交互，获取数据或者访问API时，我们就需要发送HTTP请求来进行通信。使用Haskell语言可以轻松地发送HTTP请求，并且具有高性能和强大的功能，让我们能够更加高效地进行网络通信。

## 如何

下面是一个简单的Haskell代码示例，用于发送HTTP请求并打印响应的内容：

```Haskell
import Network.HTTP.Simple

main = do
    response <- httpLBS "http://example.com"
    putStrLn (responseBody response)
```

运行这个代码，你将得到类似于下面这样的输出：

```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
    <meta charset="utf-8" />
    <meta http-equiv="Content-type" content="text/html; charset=utf-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1" />
</head>
<body>
    <div>
        <h1>Example Domain</h1>
        <p>This domain is for use in illustrative examples in documents. You may use this
        domain in literature without prior coordination or asking for permission.</p>
        <p><a href="https://www.iana.org/domains/example">More information...</a></p>
    </div>
</body>
</html>
```

这是一个简单的HTTP请求示例，它使用了Haskell网络库中的`httpLBS`函数来发送GET请求，并且使用`responseBody`函数来获取响应内容。

如果我们想要发送带有参数的POST请求，可以使用`RequestBody`和`Request`类型，例如：

```Haskell
import Network.HTTP.Simple

main = do
    let request = setRequestBodyURLEncoded [("id", "123"), ("name", "John")] "http://example.com"
    response <- httpLBS request
    putStrLn (responseBody response)
```

在这个例子中，我们使用`setRequestBodyURLEncoded`函数来构造一个包含参数的POST请求，并将其发送到指定的URL。

## 深入探讨

当我们发送HTTP请求时，还可以设置一些其他的选项，例如请求头、认证信息、代理等。让我们来看一个更加复杂的Haskell代码示例：

```Haskell
import Network.HTTP.Simple
import Network.HTTP.Client.TLS

main = do
    manager <- newTlsManager
    request <- parseRequest "https://example.com"
    let request' = setRequestMethod "POST" $ setRequestHeader "Content-Type" ["application/json"] $ setRequestBodyJSON [("username", "John"), ("password", "1234")] request
    response <- httpLbs request' manager
    putStrLn (responseBody response)
```

在这个例子中，我们使用了`HTTP.Client.TLS`模块中的`newTlsManager`函数来创建一个支持TLS的HTTP管理器。然后，我们使用`parseRequest`函数来构造一个请求对象，并使用`setRequestMethod`、`setRequestHeader`和`setRequestBodyJSON`函数来设置请求的方法、头部信息和JSON格式的请求体。最后，我们将请求和管理器对象传递给`httpLbs`函数来发送请求，并使用`responseBody`函数来获取响应内容。

总的来说，Haskell语言提供了许多丰富的网络库，可以让我们轻松地发送HTTP请求并处理响应，可以根据实际需求选择合适的库来使用。

## 参考资料

- [Haskell的网络库文档](https://hackage.haskell.org/package/http-client)
- [Haskell的HTTP请求示例](https://viric.name/entries/http-request-with-haskell)