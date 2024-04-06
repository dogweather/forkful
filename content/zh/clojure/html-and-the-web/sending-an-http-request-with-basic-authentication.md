---
date: 2024-01-20 18:01:25.716606-07:00
description: "\u5982\u4F55\u505A\uFF1A \u53D1\u9001\u5E26\u6709\u57FA\u672C\u8BA4\u8BC1\
  \u7684HTTP\u8BF7\u6C42\uFF0C\u6700\u5F00\u59CB\u662FHTTP/1.0\u7684\u4E00\u90E8\u5206\
  \uFF0C\u540E\u6210\u4E3AIETF\u6807\u51C6RFC\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:38:46.484202-06:00'
model: gpt-4-1106-preview
summary: "\u57FA\u672C\u8BA4\u8BC1\u5DE5\u4F5C\u6D41\u7A0B\u7B80\u5355\uFF1A\u5BA2\
  \u6237\u7AEF\u53D1\u9001\u5E26\u6709'Authorization'\u8BF7\u6C42\u5934\u7684HTTP\u8BF7\
  \u6C42\uFF1B\u503C\u4E3A'Basic'\u540E\u8DDF\u7A7A\u683C\u548C'\u7528\u6237\u540D\
  :\u5BC6\u7801'\u7684Base64\u7F16\u7801\u5B57\u7B26\u4E32\u3002\u5982\u679C\u8BA4\
  \u8BC1\u6210\u529F\uFF0C\u670D\u52A1\u5668\u8FD4\u56DE\u8BF7\u6C42\u8D44\u6E90\u3002\
  \u5982\u679C\u5931\u8D25\uFF0C\u670D\u52A1\u5668\u8FD4\u56DE401\u672A\u6388\u6743\
  \u54CD\u5E94\u3002"
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
