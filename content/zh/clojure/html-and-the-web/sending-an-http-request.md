---
date: 2024-01-20 17:59:19.618742-07:00
description: "\u53D1\u9001HTTP\u8BF7\u6C42\u5C31\u662F\u8BA9\u4F60\u7684\u7A0B\u5E8F\
  \u80FD\u5BF9\u7F51\u7AD9\u8FDB\u884C\u95EE\u7B54\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\
  \u505A\u4E3B\u8981\u662F\u83B7\u53D6\u6570\u636E\u6216\u4E0E\u8FDC\u7AEF\u670D\u52A1\
  \u4EA4\u4E92\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.299409-06:00'
model: gpt-4-1106-preview
summary: "\u53D1\u9001HTTP\u8BF7\u6C42\u5C31\u662F\u8BA9\u4F60\u7684\u7A0B\u5E8F\u80FD\
  \u5BF9\u7F51\u7AD9\u8FDB\u884C\u95EE\u7B54\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\
  \u4E3B\u8981\u662F\u83B7\u53D6\u6570\u636E\u6216\u4E0E\u8FDC\u7AEF\u670D\u52A1\u4EA4\
  \u4E92\u3002"
title: "\u53D1\u51FA HTTP \u8BF7\u6C42"
weight: 44
---

## What & Why?
发送HTTP请求就是让你的程序能对网站进行问答。程序员这么做主要是获取数据或与远端服务交互。

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
