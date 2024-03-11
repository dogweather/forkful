---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:56.994905-07:00
description: "YAML\u662F\u201CYAML Ain't Markup Language\u201D\uFF08YAML\u4E0D\u662F\
  \u6807\u8BB0\u8BED\u8A00\uFF09\u7684\u9012\u5F52\u7F29\u5199\uFF0C\u662F\u4E00\u79CD\
  \u4EBA\u7C7B\u53EF\u8BFB\u7684\u6570\u636E\u5E8F\u5217\u5316\u683C\u5F0F\uFF0C\u7528\
  \u4E8E\u914D\u7F6E\u6587\u4EF6\u548C\u4E0D\u540C\u6570\u636E\u7ED3\u6784\u8BED\u8A00\
  \u4E4B\u95F4\u7684\u6570\u636E\u4EA4\u6362\u3002\u7A0B\u5E8F\u5458\u5229\u7528YAML\u56E0\
  \u5176\u7B80\u5355\u6613\u8BFB\uFF0C\u4F7F\u5176\u6210\u4E3A\u914D\u7F6E\u5E94\u7528\
  \u7A0B\u5E8F\u548C\u4FC3\u8FDB\u591A\u8BED\u8A00\u7F16\u7A0B\u73AF\u5883\u4E2D\u6570\
  \u636E\u4EA4\u6362\u7684\u7406\u60F3\u9009\u62E9\u3002"
lastmod: '2024-03-11T00:14:21.096093-06:00'
model: gpt-4-0125-preview
summary: "YAML\u662F\u201CYAML Ain't Markup Language\u201D\uFF08YAML\u4E0D\u662F\u6807\
  \u8BB0\u8BED\u8A00\uFF09\u7684\u9012\u5F52\u7F29\u5199\uFF0C\u662F\u4E00\u79CD\u4EBA\
  \u7C7B\u53EF\u8BFB\u7684\u6570\u636E\u5E8F\u5217\u5316\u683C\u5F0F\uFF0C\u7528\u4E8E\
  \u914D\u7F6E\u6587\u4EF6\u548C\u4E0D\u540C\u6570\u636E\u7ED3\u6784\u8BED\u8A00\u4E4B\
  \u95F4\u7684\u6570\u636E\u4EA4\u6362\u3002\u7A0B\u5E8F\u5458\u5229\u7528YAML\u56E0\
  \u5176\u7B80\u5355\u6613\u8BFB\uFF0C\u4F7F\u5176\u6210\u4E3A\u914D\u7F6E\u5E94\u7528\
  \u7A0B\u5E8F\u548C\u4FC3\u8FDB\u591A\u8BED\u8A00\u7F16\u7A0B\u73AF\u5883\u4E2D\u6570\
  \u636E\u4EA4\u6362\u7684\u7406\u60F3\u9009\u62E9\u3002"
title: "\u4F7F\u7528YAML\u5DE5\u4F5C"
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
