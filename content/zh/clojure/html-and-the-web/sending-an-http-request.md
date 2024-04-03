---
date: 2024-01-20 17:59:19.618742-07:00
description: "How to: Clojure \u63D0\u4F9B\u4E86\u597D\u51E0\u79CD\u53D1\u8D77HTTP\
  \ \u8BF7\u6C42\u7684\u65B9\u6CD5\u3002\u8FD9\u91CC, \u6211\u4EEC\u4F1A\u7528 `clj-http`\
  \ \u5E93\u3002 \u9996\u5148, \u6DFB\u52A0\u4F9D\u8D56\u5230\u4F60\u7684 `project.clj`."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.299409-06:00'
model: gpt-4-1106-preview
summary: "Clojure \u63D0\u4F9B\u4E86\u597D\u51E0\u79CD\u53D1\u8D77HTTP \u8BF7\u6C42\
  \u7684\u65B9\u6CD5\u3002\u8FD9\u91CC, \u6211\u4EEC\u4F1A\u7528 `clj-http` \u5E93\
  ."
title: "\u53D1\u51FA HTTP \u8BF7\u6C42"
weight: 44
---

## How to:
Clojure 提供了好几种发起HTTP 请求的方法。这里, 我们会用 `clj-http` 库。

首先, 添加依赖到你的 `project.clj`:

```clojure
[clj-http "3.12.3"]
```

然后在代码中引入：

```clojure
(require '[clj-http.client :as client])
```

现在你就可以发起GET请求了：

```clojure
(def response (client/get "https://api.github.com/users/octocat"))
(println (:status response))
(println (:body response))
```

输出应该看起来像这样：

```
200
{"login":"octocat","id":583231,"node_id":"MDQ6VXNlcjU4MzIzMQ==",...}
```

## Deep Dive
HTTP请求的基础可追溯到早期的Web，由Tim Berners-Lee在1989年发明。与`clj-http`库类似的库包括`http-kit`和`aleph`。`clj-http`是对Java的`Apache HttpClient`的封装，提供了Clojure风格的接口。

## See Also
- [`clj-http` Github 仓库](https://github.com/dakrone/clj-http)
- [Clojure - 官方文件](https://clojure.org/guides/getting_started)
- [HTTP 协议概述](https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview)
