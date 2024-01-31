---
title:                "使用TOML"
date:                  2024-01-26T04:20:44.675538-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/working-with-toml.md"
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
