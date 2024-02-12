---
title:                "下载网页"
aliases:
- /zh/clojure/downloading-a-web-page/
date:                  2024-01-20T17:43:56.695245-07:00
model:                 gpt-4-1106-preview
simple_title:         "下载网页"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
下载网页，就是从互联网上获取网页内容到本地。程序员这么做，可能是为了数据分析，或者是想在本地保存网页备用。

## 如何：
```Clojure
(require '[clj-http.client :as client])

(defn download-page [url]
  (let [response (client/get url)]
    (:body response)))

(println (download-page "https://example.com"))
```
样本输出：
```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
    ...
</html>
```

## 深度挖掘
下载网页其实和早年用电话拨号上网类似，只是现在更快、更自动化。可用的库有很多，比如`clj-http`。这个库背后用的是Apache的`HttpClient`。`clj-http`相较于其他选项，比如Java内置的`HttpURLConnection`或者新的`java.net.http.HttpClient`，使用起来更简单，Clojure社区也普遍接受。

## 参见
- clj-http库：[https://github.com/dakrone/clj-http](https://github.com/dakrone/clj-http)
- Clojure官方文档：[https://clojure.org](https://clojure.org)
- Java HttpClient教程：[https://openjdk.java.net/groups/net/httpclient/intro.html](https://openjdk.java.net/groups/net/httpclient/intro.html)
