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

# 什么是发送HTTP请求及其原因？

发送HTTP请求是指通过网络协议HTTP来请求远程服务器上的资源。编程人员通常会使用发送HTTP请求来获取数据，例如从一个API接口获取数据，或者与远程服务器进行通信。

# 如何发送HTTP请求？

```Clojure
;; 首先导入Clojure内置的core.async库
(ns http-example
  (:require [clojure.core.async :as async]))
;; 使用core.async库中的<!运算符接收远程服务器的响应
(async/<! "http://example.com")
```

运行以上代码后，将会从http://example.com获取远程服务器的响应，这可以是HTML页面、JSON数据等，取决于请求的URL和远程服务器的响应。

## 深入了解

发送HTTP请求是一种常见的网络编程技术，它可以追溯到20世纪90年代。除了Clojure内置的core.async库，也可以使用其他第三方库来发送HTTP请求，例如clj-http和http-kit。

## 参考资料

https://clojure.org/reference/protocols#_sending_http_requests
https://github.com/dakrone/clj-http
https://http-kit.org