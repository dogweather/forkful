---
date: 2024-01-26 04:29:18.762955-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Clojure \u63D0\u4F9B\u4E86 `clojure.data.xml`\
  \ \u5E93\u7528\u4E8E XML \u7684\u89E3\u6790\u548C\u751F\u6210\u3002\u9996\u5148\uFF0C\
  \u8BA9\u6211\u4EEC\u6765\u89E3\u6790\u4E00\u4E9B XML\uFF1A."
lastmod: '2024-04-05T21:53:47.681384-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u5904\u7406XML"
weight: 40
---

## 如何操作：
Clojure 提供了 `clojure.data.xml` 库用于 XML 的解析和生成。首先，让我们来解析一些 XML：

```clojure
(require '[clojure.data.xml :as xml])

(let [content "<root><foo>bar</foo><foo>baz</foo></root>"
      parsed (xml/parse-str content)] ; 解析 XML 字符串
  (println parsed))
```
输出：
```
Element{:tag :root, :attrs {}, :content (Element{:tag :foo, :attrs {}, :content ("bar")} Element{:tag :foo, :attrs {}, :content ("baz")})}
```

从 Clojure 结构生成 XML：

```clojure
(def my-xml (xml/element :root {}
                          (xml/element :foo {} "bar")
                          (xml/element :foo {} "baz")))

(println (xml/emit-str my-xml))
```
输出：
```
<root><foo>bar</foo><foo>baz</foo></root>
```

## 深入了解
XML 自 90 年代末起就开始使用，它是 SGML 的一个简化子集，旨在用于 Web 数据。随着 SOAP 和 XHTML 等技术的广泛使用，XML 的使用量激增，但它也遇到了来自 JSON 的竞争，后者因其轻量和简单而受到偏好。

Clojure 对 XML 的处理方式保持功能性和以数据为中心，忠实于语言的精神。`clojure.data.xml` 只是一个选项；你还有 `clojure.xml` 用于基本需求，若需与 Java 进行互操作，你可以使用如 JAXB 或 DOM4J 这样的重量级工具。

记住，在处理非常大的 XML 文档时，性能和内存开销可能会很大。流式解析器如 StAX 可以帮助解决这个问题，但你需要为此转到 Java 领域。

## 另见
- [clojure.data.xml GitHub](https://github.com/clojure/data.xml)
- [Java API for XML Processing (JAXP)](https://docs.oracle.com/javase/tutorial/jaxp/index.html)
- [StAX](https://docs.oracle.com/javase/tutorial/jaxp/stax/index.html)
