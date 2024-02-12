---
title:                "使用YAML工作"
aliases:
- zh/clojure/working-with-yaml.md
date:                  2024-02-03T19:24:56.994905-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用YAML工作"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么？

YAML是“YAML Ain't Markup Language”（YAML不是标记语言）的递归缩写，是一种人类可读的数据序列化格式，用于配置文件和不同数据结构语言之间的数据交换。程序员利用YAML因其简单易读，使其成为配置应用程序和促进多语言编程环境中数据交换的理想选择。

## 如何操作：

Clojure没有内置对YAML的支持，但你可以利用第三方库如`clj-yaml`来解析和生成YAML数据。首先，将库添加到项目依赖中：

```clojure
;; 将此添加到你的project.clj依赖中
[clj-yaml "0.7.0"]
```

以下是如何使用`clj-yaml`来解析YAML并将Clojure映射转换为YAML的方法。

### 解析YAML：

```clojure
(require '[clj-yaml.core :as yaml])

;; 解析一个YAML字符串
(let [yaml-str "name: John Doe\nage: 30\nlanguages:\n  - Clojure\n  - Python"]
  (yaml/parse-string yaml-str))
;; 输出：
;; => {"name" "John Doe", "age" 30, "languages" ["Clojure" "Python"]}
```

### 从Clojure生成YAML：

```clojure
(require '[clj-yaml.core :as yaml])

;; 将一个Clojure映射转换为YAML字符串
(let [data-map {:name "Jane Doe" :age 28 :languages ["Java" "Ruby"]}]
  (yaml/generate-string data-map))
;; 输出：
; "age: 28\nlanguages:\n- Java\n- Ruby\nname: Jane Doe\n"
```

这些使用`clj-yaml`的简单操作可以被集成到Clojure应用程序中，以处理配置文件或促进与使用YAML的其他服务或组件的数据交换。
