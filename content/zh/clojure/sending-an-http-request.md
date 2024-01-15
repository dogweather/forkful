---
title:                "发送一个http请求。"
html_title:           "Clojure: 发送一个http请求。"
simple_title:         "发送一个http请求。"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/sending-an-http-request.md"
---

{{< edit_this_page >}}

# 为什么要发送HTTP请求？

当我们浏览网页、使用手机应用或者和服务器交互数据时，我们都在与HTTP请求打交道。通过发送HTTP请求，我们能够获取所需的数据或者与其他系统进行通信，这在现代软件开发中非常常见。

## 如何发送HTTP请求

我们可以使用Clojure的内置库`clojure.java.net`来发送HTTP请求。下面是一个简单的示例，我们将发送GET请求到一个API，并打印出响应内容：

```clojure
(require '[clojure.java.net :as http])

(defn send-request [url]
  (-> url
    (http/request)
    (http/response)
    (http/entity)
    (slurp)))

(def response (send-request "https://dog.ceo/api/breeds/image/random"))
(println response)
```

运行以上代码，我们将得到一个包含随机狗狗图片链接的字符串。

```clojure
{:data {:image "https://images.dog.ceo/breeds/doberman/n02107142_1293.jpg", :message "https://images.dog.ceo/breeds/doberman/n02107142_1293.jpg", :status "success"}}
```

## 深入了解HTTP请求

HTTP是一种客户端-服务器协议，客户端通过发送请求获得服务器端的响应。HTTP请求由请求方法、URL、请求头和可选的请求体组成。主要的请求方法包括GET、POST、PUT、DELETE等。

除了使用内置库，我们还可以使用第三方库如`clj-http`来发送HTTP请求。这些库提供了额外的特性，如错误处理和异步请求。

# 同样也看看这些

- [HTTP请求的详细文档](https://clojure.github.io/clojure/clojure.java.net-api.html)
- [Clj-http库的文档](https://clojars.org/clj-http)
- [HTTP请求的常见问题解答](https://stackoverflow.com/questions/32306827/clojure-http-request-example)