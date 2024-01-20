---
title:                "发送http请求"
html_title:           "C#: 发送http请求"
simple_title:         "发送http请求"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 是什么？为什么？

HTTP请求就是让程序从网站获取数据或者将数据推送到网站上。程序员之所以使用，是因为这是Web应用通信的标准方式。

## 如何实施：

Clojure支持使用`clj-http`库方便的发送HTTP请求。以下是一个GET请求的例子：

```Clojure
(require '[clj-http.client :as client])

(let [response (client/get "http://example.com")]
  (println (:status response)
            (:headers response)
            (:body response)))
```
运行后，你会看到类似这样的输出：

```Clojure
200
{"Date" "Wed, 15 May 2013 00:13:13 GMT", "Content-Type" "text/html; charset=utf-8", ...}
"<html>...</html>"
```

## 深入挖掘:

发送HTTP请求的工具或库已经存在几十年之久。使用`clj-http`库发送HTTP请求只是众多方法中的一种，你还可以使用Java的原生库或者其他Clojure库。

`clj-http`库实际上在Java的Apache HttpClient库基础上封装，它提供了一个简单的 API ，以便我们能够快速方便地发出 HTTP 请求。

## 还可以参考：

- clj-http Github页面: [https://github.com/dakrone/clj-http](https://github.com/dakrone/clj-http)
- HTTP 协议详解: [https://developer.mozilla.org/zh-CN/docs/Web/HTTP](https://developer.mozilla.org/zh-CN/docs/Web/HTTP)
- Java's Native HttpClient :[https://openjdk.java.net/groups/net/httpclient](https://openjdk.java.net/groups/net/httpclient)