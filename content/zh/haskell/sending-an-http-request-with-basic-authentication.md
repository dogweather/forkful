---
title:                "通过基本身份验证发送http请求"
html_title:           "Haskell: 通过基本身份验证发送http请求"
simple_title:         "通过基本身份验证发送http请求"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# 什么是基本认证HTTP请求？为什么程序员需要它？

基本认证HTTP请求是通过使用用户名和密码验证来发送HTTP请求的一种方法。程序员经常使用它来确保他们的请求只能被授权用户访问，从而保证安全性和保密性。

# 如何实现：

下面是一个示例代码，展示如何使用基本认证发送HTTP请求：

```Haskell
import Network.HTTP
import Network.HTTP.Auth

main :: IO ()
main = do
    request <- simpleHTTP (getRequest "https://example.com")
    case request of
        Left _ -> putStrLn "请求失败"
        Right response -> do
            let status = rspCode response
            case status of
                (2, _, _) -> putStrLn "请求成功"
                (3, _, _) -> do
                    let headers = rspHeaders response
                        Just authHeader = lookupHeader HdrWWWAuthenticate headers
                        challenge = parseChallenge authHeader
                        Just domain = lookup "realm" challenge
                        Just NC = lookup "nc" challenge
                        Just realm = lookup "realm" challenge
                        Just nonce = lookup "nonce" challenge
                    putStrLn ("需要认证，领域为" ++ realm ++ "，NC为" ++ NC ++ "，随机数为" ++ nonce)
                    putStrLn "请输入用户名："
                    username <- getLine
                    putStrLn "请输入密码："
                    password <- getLine
                    let auth = BasicAuth "Basic" username password
                    request' <- applyAuth auth (getRequest "https://example.com")
                    case request' of
                        Left _ -> putStrLn "认证失败"
                        Right response' -> do
                            let status' = rspCode response'
                            case status' of
                                (2, _, _) -> putStrLn "认证成功"
                                _ -> putStrLn "认证失败"
                _ -> putStrLn "请求失败"
```

运行上述代码，如果请求成功，则会打印“请求成功”，如果需要认证，则会打印相关信息并等待用户输入用户名和密码进行认证。

# 深入了解：

基本认证HTTP请求最早是在1999年被引入HTTP/1.0规范中，它的主要目的是为了提供一种简单和易于实现的认证方式。但是，它也有一些缺点，比如无法提供更高级的安全性，容易被攻击者破解用户名和密码等。因此，现在很少有人使用基本认证HTTP请求，更多的是使用OAuth等安全性更高的认证方式。

# 相关资源：

- [HTTP请求基础认证规范](https://tools.ietf.org/html/rfc7617)
- [OAuth官方文档](https://oauth.net/)
- [Haskell HTTP库文档](https://hackage.haskell.org/package/HTTP)