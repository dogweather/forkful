---
date: 2024-01-20 17:43:56.695245-07:00
description: "\u5982\u4F55\uFF1A \u4E0B\u8F7D\u7F51\u9875\u5176\u5B9E\u548C\u65E9\u5E74\
  \u7528\u7535\u8BDD\u62E8\u53F7\u4E0A\u7F51\u7C7B\u4F3C\uFF0C\u53EA\u662F\u73B0\u5728\
  \u66F4\u5FEB\u3001\u66F4\u81EA\u52A8\u5316\u3002\u53EF\u7528\u7684\u5E93\u6709\u5F88\
  \u591A\uFF0C\u6BD4\u5982`clj-http`\u3002\u8FD9\u4E2A\u5E93\u80CC\u540E\u7528\u7684\
  \u662FApache\u7684`HttpClient`\u3002`clj-\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:47.653076-06:00'
model: gpt-4-1106-preview
summary: ''
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
