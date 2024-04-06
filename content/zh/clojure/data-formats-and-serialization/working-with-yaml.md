---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:56.994905-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Clojure\u6CA1\u6709\u5185\u7F6E\u5BF9\
  YAML\u7684\u652F\u6301\uFF0C\u4F46\u4F60\u53EF\u4EE5\u5229\u7528\u7B2C\u4E09\u65B9\
  \u5E93\u5982`clj-yaml`\u6765\u89E3\u6790\u548C\u751F\u6210YAML\u6570\u636E\u3002\
  \u9996\u5148\uFF0C\u5C06\u5E93\u6DFB\u52A0\u5230\u9879\u76EE\u4F9D\u8D56\u4E2D\uFF1A\
  ."
lastmod: '2024-04-05T21:53:47.677048-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u4F7F\u7528YAML\u5DE5\u4F5C"
weight: 41
---

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
