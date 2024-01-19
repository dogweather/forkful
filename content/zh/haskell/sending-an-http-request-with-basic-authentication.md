---
title:                "使用基本认证发送http请求"
html_title:           "Bash: 使用基本认证发送http请求"
simple_title:         "使用基本认证发送http请求"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Haskell中的HTTP请求与基本认证

## 什么 & 为什么？

在网络编程中，HTTP请求与基本认证是使用用户名和密码访问受保护的网络资源的一种常见方式。程序员通过它来保护用户的数据，并控制对特定资源的访问。

## 如何实现？

在Haskell中，我们可以使用http-conduit库来发送带有基本认证的HTTP请求。这是一个使用的简单例子。

```Haskell
import Network.HTTP.Simple
import Network.HTTP.Client

main :: IO ()
main = do
    let settings = applyBasicAuth "username" "password"
    request' <- parseRequest "http://example.com"
    let request = setRequestManager (settings defaultManagerSettings) request'
    response <- httpLBS request

    putStrLn $ "The status code is: " ++ show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    L8.putStrLn $ getResponseBody response
```

在这个例子中，我们引入`Network.HTTP.Simple`和`Network.HTTP.Client`库，创建了一个基本的认证设置，然后设置了一个HTTP请求。展示结果时，我们获取了响应的状态码和头信息，并打印了响应的主体。

## 深入探讨

在Web的早期，基本认证是一种常见的身份验证方式。尽管现在已经有了更安全、更灵活的认证方式，例如OAuth和JWT，但基本认证在许多情况下仍然是一个有效的选择，比如在内部网络或不需要用户交互的服务器到服务器的通信中。

http-conduit库提供了一种非常高效的方法来处理HTTP请求。它提供了大量的配置选项，比如代理设置、超时设置等。在处理基本认证时，也提供了很好的支持。除此之外，Haskell社区还有其他一些库可用于处理HTTP请求，如http-client、Wreq等。

## 另请参阅

相关资料和进一步阅读：

1. http-conduit库在线文档：https://hackage.haskell.org/package/http-conduit
2. Haskell中的网络编程教程：http://book.realworldhaskell.org/read/using-parsec.html
3. Haskell中HTTP请求的深入介绍：https://www.schoolofhaskell.com/user/adinapoli/the-pragmatic-haskeller
4. 关于HTTP的基本认证的RFC文档：https://tools.ietf.org/html/rfc7617