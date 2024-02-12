---
title:                "发出 HTTP 请求"
aliases: - /zh/clojure/sending-an-http-request.md
date:                  2024-01-20T17:59:19.618742-07:00
model:                 gpt-4-1106-preview
simple_title:         "发出 HTTP 请求"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/sending-an-http-request.md"
---

{{< edit_this_page >}}

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
