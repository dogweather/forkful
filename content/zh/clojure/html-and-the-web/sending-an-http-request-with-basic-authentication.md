---
title:                "使用基本认证发送 HTTP 请求"
date:                  2024-01-20T18:01:25.716606-07:00
model:                 gpt-4-1106-preview
simple_title:         "使用基本认证发送 HTTP 请求"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
发送带有基本认证的HTTP请求是在请求中包含用户名和密码以访问受保护的资源。程序员这样做来确保只有授权的用户可以访问敏感数据或执行操作。

## 如何做：
```Clojure
(require '[clj-http.client :as client])

(defn send-authenticated-request [url username password]
  (let [credentials (str username ":" password)
        encoded-credentials (java.util.Base64/encoder (clojure.string/bytes credentials))
        auth-header {"Authorization" (str "Basic " encoded-credentials)}]
    (client/get url {:headers auth-header})))

;; 使用样例
(def url "https://protected.example.com")
(def username "user1")
(def password "pass123")

(println (send-authenticated-request url username password))
```

示例输出：
```
{:status 200, :body "呼叫成功"}
```

## 深入探索
发送带有基本认证的HTTP请求，最开始是HTTP/1.0的一部分，后成为IETF标准RFC 7617的规定。除了基本认证，还有摘要认证、OAuth等更安全的方法。基本认证的最大问题是用户名和密码以Base64编码方式发送，如果不是HTTPS请求，则很容易被拦截。因此，现代应用中，基本认证通常只用于内部系统或需要快速原型的场合。

基本认证工作流程简单：客户端发送带有'Authorization'请求头的HTTP请求；值为'Basic'后跟空格和'用户名:密码'的Base64编码字符串。如果认证成功，服务器返回请求资源。如果失败，服务器返回401未授权响应。

使用Clojure进行带有基本认证的HTTP请求时可以利用clj-http库，这个库提供了简单而强大的HTTP客户端功能。记住处理好安全问题，特别是在生产环境下使用时。 

## 参考
- Clojure clj-http库: [https://github.com/dakrone/clj-http](https://github.com/dakrone/clj-http)
- IETF RFC 7617（基本认证的规则说明）: [https://tools.ietf.org/html/rfc7617](https://tools.ietf.org/html/rfc7617)
- 关于HTTP认证方式的比较: [https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
