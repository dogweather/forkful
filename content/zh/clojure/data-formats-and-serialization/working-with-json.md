---
title:                "使用JSON进行编程"
aliases: - /zh/clojure/working-with-json.md
date:                  2024-02-03T19:22:04.960170-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用JSON进行编程"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么与为什么？
在Clojure中处理JSON（JavaScript对象表示法）包括将JSON字符串解析成Clojure数据结构（映射，向量）及其相反操作。这项任务对于需要用结构化、基于文本的格式通信数据的网络服务、API以及应用来说是基本的，因为JSON在不同的编程环境中被普遍认可并支持。

## 如何操作：
Clojure没有内置的处理JSON的函数，因此你通常会使用第三方库。`cheshire`和`jsonista`是由于它们的易用性和性能而受欢迎的选择。

### 使用Cheshire
首先，在`project.clj`中将Cheshire添加到你的项目依赖中：
```clj
[com.fasterxml.jackson.core/jackson-core "2.12.0"]
[cheshire "5.10.1"]
```

解析JSON字符串到Clojure映射以及将映射转换为JSON字符串：

```clj
(require '[cheshire.core :as json])

;; 将JSON字符串解析为Clojure映射
(let [json-input "{\"name\":\"John\", \"age\":30}"]
  (json/parse-string json-input true)) ; => {"name" "John", "age" 30}

;; 将Clojure映射转换为JSON字符串
(let [clj-map {"name" "John", "age" 30}]
  (json/generate-string clj-map)) ; => "{\"name\":\"John\",\"age\":30}"
```

### 使用Jsonista
在`project.clj`中添加Jsonista到你的项目：
```clj
[jsonista "0.3.2"]
```

与Jsonista进行类似的操作：

```clj
(require '[jsonista.core :as j])

;; 将JSON字符串解析为Clojure
(let [json-input "{\"name\":\"Emily\", \"age\":25}"]
  (j/read-value json-input)) ; => {"name" "Emily", "age" 25}

;; 将Clojure映射转换为JSON字符串
(let [clj-map {"name" "Emily", "age" 25}]
  (j/write-value-as-string clj-map)) ; => "{\"name\":\"Emily\",\"age\":25}"
```

在这两个库中，你可以选择编码和解码更复杂的数据结构，还有其他函数和参数允许你自定义序列化和反序列化过程。对于大多数应用来说，演示的功能为在Clojure应用中处理JSON提供了坚实的基础。
