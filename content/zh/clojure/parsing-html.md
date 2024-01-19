---
title:                "解析HTML"
html_title:           "Clojure: 解析HTML"
simple_title:         "解析HTML"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/parsing-html.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?

解析HTML就是提取HTML文档中的有用信息。程序员之所以解析HTML，是因为这样能更有效地抓取网页数据。

## 如何做:

Clojure 允许我们使用Enlive库轻松解析HTML。首先，让我们添加Enlive依赖项到项目中。

```Clojure
(defproject your-project "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [net.cgrand/enlive "1.1.6"]])
```

接下来, 我们将会解析一个包含书籍列表HTML的概述。

```Clojure
(ns your-namespace
  (:require
   [net.cgrand.enlive-html :as html]
   [clojure.string :as str]))

(defn parse-html [html-source]
  (->> html-source
       html/html-resource
       (html/select [:div.book])
       (map (fn [div]
              (let [title (-> div (html/select [:h2]) first :content)
                    author (-> div (html/select [:span.author]) first :content)]
                {:title (str/join "" title)
                 :author (str/join "" author)})))))
```

函数 `parse-html` 接收一个HTML源文件，并返回一个包含书名和作者的映射列表。

## 深度剖析:

在Clojure序列处理和函数编程的指导下，HTML解析因其音质而备受赞誉。尽管现有许多其它库，如jsoup和java-html-parser，但Enlive的强大表达力、对HTML解构的出色支持，以及较小的性能损失使它成为Clojure中首选的HTML解析库。

然而，这并不是没有问题的。Enlive旨在提供一个声明性的HTML转换引擎，有些用户可能会发现其API有点复杂和难以理解。

## 延伸阅读:

[Enlive GitHub](https://github.com/cgrand/enlive)
[Clojure官方源](https://clojure.org/)
[jsoup官方文档](https://jsoup.org/)
[java-html-parser官方源](https://htmlparser.info/)