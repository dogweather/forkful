---
title:                "Haskell: 用基本身份验证发送http请求"
simple_title:         "用基本身份验证发送http请求"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# 为什么

发送带有基本认证的HTTP请求是一种安全的方法，可以用来访问需要身份验证的网站或API。这样可以保证用户的数据和隐私不会被未经授权的访问。

# 如何操作

要发送带有基本认证的HTTP请求，可以使用Haskell中的网络库，例如"Wreq"。首先，我们需要导入"Wreq"库：

```Haskell
import Network.Wreq
```

然后，我们可以使用"Wreq"中的"getWith"函数来发送一个HTTP GET请求，带有认证信息。需要提供请求的URL，基本认证的用户名和密码：

```Haskell
r <- getWith auth "https://example.com"
  where
    auth = basicAuth "username" "password"
```

最后，我们可以通过读取响应体来获取返回的数据：

```Haskell
responseBody r
```

发送带有基本认证的HTTP POST请求也是类似的，只需要使用"Wreq"中的"postWith"函数。
 
# 深入了解

基本认证是通过在请求头中添加"Authorization"字段来实现的。这个字段包含基本认证的用户名和密码的编码。在Haskell中，我们可以使用"Hackage"中的"base64-bytestring"库来对用户名和密码进行编码。需要先将用户名和密码连接起来，然后对结果进行编码，并添加到请求头中。

# 另请参阅

- [Haskell官方文档](https://www.haskell.org/documentation/)
- [Wreq包文档](https://hackage.haskell.org/package/wreq)
- [Base64-bytestring包文档](https://hackage.haskell.org/package/base64-bytestring)