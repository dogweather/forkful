---
date: 2024-01-20 17:43:56.695245-07:00
description: "\u4E0B\u8F7D\u7F51\u9875\uFF0C\u5C31\u662F\u4ECE\u4E92\u8054\u7F51\u4E0A\
  \u83B7\u53D6\u7F51\u9875\u5185\u5BB9\u5230\u672C\u5730\u3002\u7A0B\u5E8F\u5458\u8FD9\
  \u4E48\u505A\uFF0C\u53EF\u80FD\u662F\u4E3A\u4E86\u6570\u636E\u5206\u6790\uFF0C\u6216\
  \u8005\u662F\u60F3\u5728\u672C\u5730\u4FDD\u5B58\u7F51\u9875\u5907\u7528\u3002"
isCJKLanguage: true
lastmod: '2024-02-25T18:49:44.935437-07:00'
model: gpt-4-1106-preview
summary: "\u4E0B\u8F7D\u7F51\u9875\uFF0C\u5C31\u662F\u4ECE\u4E92\u8054\u7F51\u4E0A\
  \u83B7\u53D6\u7F51\u9875\u5185\u5BB9\u5230\u672C\u5730\u3002\u7A0B\u5E8F\u5458\u8FD9\
  \u4E48\u505A\uFF0C\u53EF\u80FD\u662F\u4E3A\u4E86\u6570\u636E\u5206\u6790\uFF0C\u6216\
  \u8005\u662F\u60F3\u5728\u672C\u5730\u4FDD\u5B58\u7F51\u9875\u5907\u7528\u3002"
title: "\u4E0B\u8F7D\u7F51\u9875"
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
