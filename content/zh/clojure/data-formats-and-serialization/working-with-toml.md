---
aliases:
- /zh/clojure/working-with-toml/
date: 2024-01-26 04:20:44.675538-07:00
description: "\u4F7F\u7528TOML\u610F\u5473\u7740\u4F60\u6B63\u5728\u5904\u7406\u6570\
  \u636E\u91C7\u7528\u4E86\"Tom\u7684\u660E\u663E\u7684\u3001\u6700\u5C0F\u5316\u8BED\
  \u8A00\"\uFF08Tom's Obvious, Minimal Language\uFF09\u683C\u5F0F\uFF0C\u8FD9\u79CD\
  \u683C\u5F0F\u56E0\u5176\u6613\u4E8E\u9605\u8BFB\u800C\u88AB\u5E7F\u6CDB\u7528\u4E8E\
  \u914D\u7F6E\u6587\u4EF6\u3002\u7A0B\u5E8F\u5458\u4F7F\u7528\u5B83\u6765\u8FDB\u884C\
  \u76F4\u63A5\u660E\u4E86\u7684\u914D\u7F6E\u7BA1\u7406\uFF0C\u5B83\u642D\u8F7D\u4E86\
  \u4EBA\u6027\u53CB\u597D\u7684\u8BED\u6CD5\uFF0C\u53EF\u4EE5\u5373\u63D2\u5373\u7528\
  \u3002"
lastmod: 2024-02-18 23:08:58.849275
model: gpt-4-0125-preview
summary: "\u4F7F\u7528TOML\u610F\u5473\u7740\u4F60\u6B63\u5728\u5904\u7406\u6570\u636E\
  \u91C7\u7528\u4E86\"Tom\u7684\u660E\u663E\u7684\u3001\u6700\u5C0F\u5316\u8BED\u8A00\
  \"\uFF08Tom's Obvious, Minimal Language\uFF09\u683C\u5F0F\uFF0C\u8FD9\u79CD\u683C\
  \u5F0F\u56E0\u5176\u6613\u4E8E\u9605\u8BFB\u800C\u88AB\u5E7F\u6CDB\u7528\u4E8E\u914D\
  \u7F6E\u6587\u4EF6\u3002\u7A0B\u5E8F\u5458\u4F7F\u7528\u5B83\u6765\u8FDB\u884C\u76F4\
  \u63A5\u660E\u4E86\u7684\u914D\u7F6E\u7BA1\u7406\uFF0C\u5B83\u642D\u8F7D\u4E86\u4EBA\
  \u6027\u53CB\u597D\u7684\u8BED\u6CD5\uFF0C\u53EF\u4EE5\u5373\u63D2\u5373\u7528\u3002"
title: "\u4F7F\u7528TOML"
---

{{< edit_this_page >}}

## 什么与为什么？
使用TOML意味着你正在处理数据采用了"Tom的明显的、最小化语言"（Tom's Obvious, Minimal Language）格式，这种格式因其易于阅读而被广泛用于配置文件。程序员使用它来进行直接明了的配置管理，它搭载了人性友好的语法，可以即插即用。

## 如何操作：
要在Clojure中使用TOML，你需要一个像`clj-toml`这样的库。首先，将它添加到你的`deps.edn`中：

```clojure
{:deps {clj-toml {:mvn/version "0.5.0"}}}
```

然后解析一些TOML：

```clojure
(require '[clj-toml.core :as toml])

(def config-str "title = 'TOML 示例'")

(def parsed-config (toml/parse-string config-str))

;; 从解析后的TOML中获取标题
(println (:title parsed-config)) ;; 输出：TOML 示例
```

生成TOML：

```clojure
(def data {:title "TOML 示例"})

(println (toml/generate-string data))
;; 输出：title = "TOML 示例"
```

## 深入探讨
TOML由GitHub的共同创始人Tom Preston-Werner于2013年左右创建，作为配置文件的YAML和JSON的一个更简单的替代品。它旨在清晰明了，意图使得人们无需额外工具就能读懂规范。

虽然JSON经常用于API和网络应用，而YAML可能因引用和脚本能力变得复杂，但TOML以其简单、基于表的结构脱颖而出。这种简单性尤其在Rust社区以及其他现代语言环境中广受欢迎。

Clojure，以其对简单性和实用性的关注，与TOML的配置相得益彰。`clj-toml`或其他替代库弥合了差距。它们将TOML的静态数据翻译成Clojure的动态、函数式世界。

## 参见
- TOML的GitHub仓库：[github.com/toml-lang/toml](https://github.com/toml-lang/toml)
- `clj-toml`在Clojars上：[clojars.org/clj-toml](https://clojars.org/clj-toml)
- Clojure文档：[clojure.org](https://clojure.org/guides/getting_started)
- `clj-toml`介绍：[github.com/lantiga/clj-toml](https://github.com/lantiga/clj-toml)
