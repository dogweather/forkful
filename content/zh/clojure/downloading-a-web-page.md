---
title:                "Clojure: 下载网页。"
simple_title:         "下载网页。"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 为什么？

在当今互联网时代，我们经常需要从网页中获取数据来进行分析、处理或展示。因此，学习如何使用Clojure来下载网页是非常有价值的技能，它能帮助我们更有效地获取我们需要的数据。

## 如何做？

首先，我们需要导入Clojure的一个库，叫做clj-http。它提供了一些函数来帮助我们发送和接收HTTP请求。

```Clojure
(ns my-clojure-project 
  (:require [clj-http.client :as http]))
```

然后，我们可以使用 `GET` 函数来获取网页的内容。假设我们想要下载Wikipedia的主页，我们可以像下面这样写：

```Clojure
(def wikipedia-page (http/get "https://zh.wikipedia.org/"))
```

如果我们想要查看页面的返回状态码，可以使用 `:status` 关键字：

```Clojure
(:status wikipedia-page) ; 返回 200 表示成功
```

如果我们想要获取网页的HTML源代码，可以使用 `:body` 关键字：

```Clojure
(:body wikipedia-page) ; 返回一个字符串，表示页面的HTML源代码
```

## 深入挖掘

除了基本的 `GET` 函数外，clj-http 还提供了很多其他有用的函数，例如 `POST`、`PUT`、`DELETE` 等，可以帮助我们发送不同类型的HTTP请求。

另外，我们还可以使用一些参数来定制我们的请求，例如添加请求头、设置超时时间等。具体的使用方法可以参考 clj-http 的官方文档。

## 参考链接

- clj-http 官方文档: https://github.com/dakrone/clj-http
- Clojure 官网: https://clojure.org/
- 学习 Clojure 的免费资源: https://purelyfunctional.tv/guide/best-practices-learning-clojure/zh-cn/