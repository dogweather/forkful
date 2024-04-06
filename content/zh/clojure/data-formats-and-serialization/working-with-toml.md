---
date: 2024-01-26 04:20:44.675538-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u8981\u5728Clojure\u4E2D\u4F7F\u7528\
  TOML\uFF0C\u4F60\u9700\u8981\u4E00\u4E2A\u50CF`clj-toml`\u8FD9\u6837\u7684\u5E93\
  \u3002\u9996\u5148\uFF0C\u5C06\u5B83\u6DFB\u52A0\u5230\u4F60\u7684`deps.edn`\u4E2D\
  \uFF1A."
lastmod: '2024-04-05T21:53:47.680474-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u4F7F\u7528TOML"
weight: 39
---

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
