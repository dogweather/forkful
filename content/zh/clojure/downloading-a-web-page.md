---
title:                "下载网页"
html_title:           "Arduino: 下载网页"
simple_title:         "下载网页"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?

下载网页是从服务器获取网页内容的过程。程序员进行这个操作是为了抓取、分析网页数据，或者创建网页的备份。

## 如何做:

下载网页可以使用clojure的http-kit客户端。这是一个简单、强大的HTTP库。如下是下载Google主页的代码示例:

```Clojure
(require '[org.httpkit.client :as http])

(defn download-webpage 
  [url]
  (let [response @(http/get url {:as :text})]
    (:body response)))

;; 使用方法
(save-webpage "http://google.com")
```

基本上，`http/get`函数向给定URL发出GET请求。`@`操作会阻止该函数，直到请求返回结果，然后我们只取返回体中的内容。

## 深度探究：

首先，下载网页的需求源自网页抓取，这是早期网络索引的基础。早在90年代，搜索引擎就通过这种方式收集网页。
其次，Clojure的其他库如clj-http和http.async.client也能进行网页下载，选择哪种取决于你的具体需求。
最后，为了解决网络延迟问题，http-kit使用了基于回调的异步设计，该设计能提高程序的性能。

## 参考资料： 

1. http-kit文档: http://http-kit.org/400.html
2. clj-http文档: https://github.com/dakrone/clj-http
3. http.async.client文档: https://github.com/neotyk/http.async.client
4. Web抓取相关资料: https://en.wikipedia.org/wiki/Web_scraping