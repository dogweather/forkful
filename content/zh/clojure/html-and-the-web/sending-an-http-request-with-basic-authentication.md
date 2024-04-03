---
date: 2024-01-20 18:01:25.716606-07:00
description: "\u53D1\u9001\u5E26\u6709\u57FA\u672C\u8BA4\u8BC1\u7684HTTP\u8BF7\u6C42\
  \u662F\u5728\u8BF7\u6C42\u4E2D\u5305\u542B\u7528\u6237\u540D\u548C\u5BC6\u7801\u4EE5\
  \u8BBF\u95EE\u53D7\u4FDD\u62A4\u7684\u8D44\u6E90\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\
  \u505A\u6765\u786E\u4FDD\u53EA\u6709\u6388\u6743\u7684\u7528\u6237\u53EF\u4EE5\u8BBF\
  \u95EE\u654F\u611F\u6570\u636E\u6216\u6267\u884C\u64CD\u4F5C\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.302448-06:00'
model: gpt-4-1106-preview
summary: "\u53D1\u9001\u5E26\u6709\u57FA\u672C\u8BA4\u8BC1\u7684HTTP\u8BF7\u6C42\u662F\
  \u5728\u8BF7\u6C42\u4E2D\u5305\u542B\u7528\u6237\u540D\u548C\u5BC6\u7801\u4EE5\u8BBF\
  \u95EE\u53D7\u4FDD\u62A4\u7684\u8D44\u6E90\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\
  \u6765\u786E\u4FDD\u53EA\u6709\u6388\u6743\u7684\u7528\u6237\u53EF\u4EE5\u8BBF\u95EE\
  \u654F\u611F\u6570\u636E\u6216\u6267\u884C\u64CD\u4F5C\u3002."
title: "\u4F7F\u7528\u57FA\u672C\u8BA4\u8BC1\u53D1\u9001 HTTP \u8BF7\u6C42"
weight: 45
---

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
