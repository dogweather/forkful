---
title:                "用基本身份验证发送http请求"
html_title:           "Clojure: 用基本身份验证发送http请求"
simple_title:         "用基本身份验证发送http请求"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# 为什么

在Web开发中，我们经常需要发送HTTP请求以与服务器通信。使用基本认证（Basic Authentication）可以确保我们的请求是安全和经过授权的，因此非常重要。

# 如何使用

我们可以使用Clojure中的```clojure (clj-http)```库来发送HTTP请求。首先，我们需要导入该库以及我们需要使用的其他库。

```clojure
(require '[clj-http.client :as client])
(require '[clojure.data.json :as json])
```

然后，我们可以使用```client/basic-auth```函数来为请求添加基本认证。该函数需要两个参数：用户名和密码。下面是一个发送GET请求的例子：

```clojure
(def response (client/get "https://example.com" {:basic-auth ["USERNAME" "PASSWORD"]}))
```

我们可以使用```(:headers response)```来查看响应头部信息，使用```(:status response)```来查看状态码，使用```(:body response)```来查看具体内容。

# 深入了解

基本认证是一种最简单的HTTP认证方式。它使用用户名和密码作为凭证，并将其以Base64编码的形式传输到服务器。服务器会验证凭证是否正确，并返回相应的响应码。虽然这种方式足够简单，但是它并不安全，因为凭证在传输过程中是明文可见的。

# 参考链接

- [clj-http官方文档](https://github.com/dakrone/clj-http)
- [HTTP基本认证（Basic Authentication）](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [Clojure中文网](https://www.clojure.cn/)