---
title:                "处理JSON数据"
date:                  2024-01-19
simple_title:         "处理JSON数据"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/working-with-json.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
JSON是一种数据格式，用于存储和传输数据。程序员处理JSON来进行数据交换，因为它易于人阅读也易于机器解析。

## 如何操作：
```Clojure
;; 添加Clojure的JSON库
(require '[cheshire.core :as json])

;; 解析JSON字符串
(def json-str "{\"name\":\"John\", \"age\":30}")
(def data (json/parse-string json-str))
(println data)

;; 生成JSON字符串
(def clojure-map {:name "John", :age 30})
(def json-output (json/generate-string clojure-map))
(println json-output)
```
输出：
```Clojure
{"name" "John", "age" 30}
{"name":"John","age":30}
```

## 深入探索
JSON，全称是JavaScript Object Notation，起源于JavaScript，但现在被各种编程语言广泛支持。Clojure作为一种现代Lisp方言，提供了像Cheshire这样的库来简化JSON的解析和生成。虽然有其他格式（如XML）可供选择，但JSON因其简洁性和高效性成为Web服务中的主流。具体到Clojure实现，像`json/parse-string`和`json/generate-string`这些函数利用Java虚拟机优势进行快速且安全的数据处理。

## 另请参阅
- Clojure官方文档：[https://clojure.org/](https://clojure.org/)
- Cheshire库文档：[https://github.com/dakrone/cheshire](https://github.com/dakrone/cheshire)
- JSON官方网站：[https://www.json.org/json-en.html](https://www.json.org/json-en.html)
