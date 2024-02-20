---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:04.960170-07:00
description: "\u5728Clojure\u4E2D\u5904\u7406JSON\uFF08JavaScript\u5BF9\u8C61\u8868\
  \u793A\u6CD5\uFF09\u5305\u62EC\u5C06JSON\u5B57\u7B26\u4E32\u89E3\u6790\u6210Clojure\u6570\
  \u636E\u7ED3\u6784\uFF08\u6620\u5C04\uFF0C\u5411\u91CF\uFF09\u53CA\u5176\u76F8\u53CD\
  \u64CD\u4F5C\u3002\u8FD9\u9879\u4EFB\u52A1\u5BF9\u4E8E\u9700\u8981\u7528\u7ED3\u6784\
  \u5316\u3001\u57FA\u4E8E\u6587\u672C\u7684\u683C\u5F0F\u901A\u4FE1\u6570\u636E\u7684\
  \u7F51\u7EDC\u670D\u52A1\u3001API\u4EE5\u53CA\u5E94\u7528\u6765\u8BF4\u662F\u57FA\
  \u672C\u7684\uFF0C\u56E0\u4E3AJSON\u5728\u4E0D\u540C\u7684\u7F16\u7A0B\u73AF\u5883\
  \u4E2D\u88AB\u666E\u904D\u8BA4\u53EF\u5E76\u652F\u6301\u3002"
lastmod: 2024-02-19 22:05:06.411595
model: gpt-4-0125-preview
summary: "\u5728Clojure\u4E2D\u5904\u7406JSON\uFF08JavaScript\u5BF9\u8C61\u8868\u793A\
  \u6CD5\uFF09\u5305\u62EC\u5C06JSON\u5B57\u7B26\u4E32\u89E3\u6790\u6210Clojure\u6570\
  \u636E\u7ED3\u6784\uFF08\u6620\u5C04\uFF0C\u5411\u91CF\uFF09\u53CA\u5176\u76F8\u53CD\
  \u64CD\u4F5C\u3002\u8FD9\u9879\u4EFB\u52A1\u5BF9\u4E8E\u9700\u8981\u7528\u7ED3\u6784\
  \u5316\u3001\u57FA\u4E8E\u6587\u672C\u7684\u683C\u5F0F\u901A\u4FE1\u6570\u636E\u7684\
  \u7F51\u7EDC\u670D\u52A1\u3001API\u4EE5\u53CA\u5E94\u7528\u6765\u8BF4\u662F\u57FA\
  \u672C\u7684\uFF0C\u56E0\u4E3AJSON\u5728\u4E0D\u540C\u7684\u7F16\u7A0B\u73AF\u5883\
  \u4E2D\u88AB\u666E\u904D\u8BA4\u53EF\u5E76\u652F\u6301\u3002"
title: "\u4F7F\u7528JSON\u8FDB\u884C\u7F16\u7A0B"
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
