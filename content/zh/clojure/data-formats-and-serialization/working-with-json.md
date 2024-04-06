---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:04.960170-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Clojure\u6CA1\u6709\u5185\u7F6E\u7684\
  \u5904\u7406JSON\u7684\u51FD\u6570\uFF0C\u56E0\u6B64\u4F60\u901A\u5E38\u4F1A\u4F7F\
  \u7528\u7B2C\u4E09\u65B9\u5E93\u3002`cheshire`\u548C`jsonista`\u662F\u7531\u4E8E\
  \u5B83\u4EEC\u7684\u6613\u7528\u6027\u548C\u6027\u80FD\u800C\u53D7\u6B22\u8FCE\u7684\
  \u9009\u62E9\u3002 \u9996\u5148\uFF0C\u5728`project.clj`\u4E2D\u5C06Cheshire\u6DFB\
  \u52A0\u5230\u4F60\u7684\u9879\u76EE\u4F9D\u8D56\u4E2D\uFF1A."
lastmod: '2024-03-13T22:44:47.328143-06:00'
model: gpt-4-0125-preview
summary: "Clojure\u6CA1\u6709\u5185\u7F6E\u7684\u5904\u7406JSON\u7684\u51FD\u6570\uFF0C\
  \u56E0\u6B64\u4F60\u901A\u5E38\u4F1A\u4F7F\u7528\u7B2C\u4E09\u65B9\u5E93\u3002`cheshire`\u548C\
  `jsonista`\u662F\u7531\u4E8E\u5B83\u4EEC\u7684\u6613\u7528\u6027\u548C\u6027\u80FD\
  \u800C\u53D7\u6B22\u8FCE\u7684\u9009\u62E9."
title: "\u4F7F\u7528JSON\u8FDB\u884C\u7F16\u7A0B"
weight: 38
---

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
