---
title:                "使用基本认证发送http请求"
html_title:           "Bash: 使用基本认证发送http请求"
simple_title:         "使用基本认证发送http请求"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 什么与为什么?

发送带有基本认证的HTTP请求，是一种通过网络发送信息并接收响应的方法。程序员会这么做，是因为这可以让他们访问，获取并操作受保护的资源。

## 如何操作:

在 Clojure 中，你需要利用 clj-http 库来进行 HTTP 请求。你可以在项目.clj中添加：`[clj-http "3.12.0"]` 来导入这个库。下面是一个带有基本认证的 GET 请求的示例：

```Clojure
(use 'clj-http.client)

(defn send-request []
  (let [auth {:basic ["username" "password"]}
        response (get "http://example.com" {:auth auth})]
    (:body responce)))

(println (send-request))
```

输出可能像这样：

```Clojure
"Hello, World!"
```

这里，basicauth 是一种含有username和password的认证方式。响应体可通过 `(:body response)` 来获取。

## 深入了解

发送带有基本认证的 HTTP 请求是在 HTTP/1.0 规范中引入的。在没有 SSL/TLS 的情况下，它提供了最基本的安全性，可防止普通的嗅探者获取用户名和密码。

替代方案包括 OAuth, JWT，它们提供了更安全的认证方式。然而，对于安全性要求不高，需要快速实现简单认证的场景，基本认证仍然是个不错的选择。

对于进行http请求的底层实现， clj-http 使用 java的 Apache HttpClient 库。你可以在代码中对 HTTP 请求细节进行高度自定义，例如连接超时，编码方式等。

## 参考资料

1. clj-http文档: [https://github.com/dakrone/clj-http](https://github.com/dakrone/clj-http)
2. HttpRequest基本认证 wiki: [https://en.wikipedia.org/wiki/Basic_access_authentication](https://en.wikipedia.org/wiki/Basic_access_authentication)