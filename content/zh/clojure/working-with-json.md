---
title:                "使用json进行编程"
html_title:           "Clojure: 使用json进行编程"
simple_title:         "使用json进行编程"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/working-with-json.md"
---

{{< edit_this_page >}}

什么是JSON？为什么程序员要使用它？

JSON（JavaScript Object Notation）是一种轻量级的数据交换格式，它被广泛用于在不同系统之间传输数据。程序员经常使用JSON来从服务器或其他系统中请求和接收数据。

如何使用JSON：

```Clojure
(require '[clj-http.client :as client])
(require '[cheshire.core :as json])

(def response (client/get "https://api.github.com/users/github"))

(def user-info (json/parse-string (:body response)))

(println "User's name: " (:name user-info))
(println "User's bio: " (:bio user-info))
```

在这个示例中，我们使用了clj-http和cheshire这两个库来发送请求并解析返回的JSON数据。首先，我们使用clj-http来发出GET请求并获取响应。然后，我们使用cheshire的parse-string函数来将响应体转换为Clojure的map数据结构。最后，我们可以从map中提取出我们需要的数据，比如用户的名字和简介。

深入了解：

JSON起源于JavaScript语言，但现在已经成为一种跨语言通用的数据格式。除了Clojure，其他语言也有许多支持JSON的第三方库。另外，肯定有一些不同的方式来处理JSON，但上面提到的两个库是最常用的。如果你想进一步学习如何在Clojure中使用JSON，可以参考以下资源：

- [clj-http官方文档](https://github.com/dakrone/clj-http)
- [cheshire官方文档](https://github.com/dakrone/cheshire)
- [Clojure中处理JSON的其他方法](https://stackoverflow.com/questions/14416587/how-to-handle-json-in-clojure)

相关链接：

- [JSON官方文档](https://www.json.org/json-en.html)
- [JSON - 维基百科](https://zh.wikipedia.org/zh-hans/JSON)