---
date: 2024-01-20 17:43:56.695245-07:00
description: "\u5982\u4F55\uFF1A ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.301429-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u4E0B\u8F7D\u7F51\u9875"
weight: 42
---

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
