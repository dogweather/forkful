---
title:                "下载网页"
html_title:           "Clojure: 下载网页"
simple_title:         "下载网页"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 为什么

下载网页是一个非常常见的操作，可以用来获取网页的内容、数据或者图片。无论是进行数据分析、制作网页备份还是获取信息，都会有下载网页的需求。

## 如何

在Clojure中，可以使用 `clj-http` 库来下载网页。首先，需要在项目中引入这个库：

```
[clj-http "3.7.0"]
```

然后，使用 `client/get` 函数来下载网页，指定网页的URL，并将结果保存在一个变量中：

```
(def page (client/get "https://www.example.com"))
```

通过 `:body` 键获取网页内容，在控制台上打印网页的内容：

```
(println (:body page))
```

最后，关闭连接，以释放资源：

```
(client/close page)
```

## 深入了解

除了基本的下载功能，`clj-http` 还提供了许多其他选项，例如可选的HTTP头、代理设置、错误处理等。可以参考官方文档来了解更多详情。

## 参考链接

* [Clojure官方网站](https://clojure.org/)
* [clj-http文档](https://github.com/dakrone/clj-http)
* [Clojure社区论坛](https://clojureverse.org/)