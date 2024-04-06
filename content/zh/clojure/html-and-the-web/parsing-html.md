---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:46.605276-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Clojure\u6CA1\u6709\u5185\u7F6E\u7684\
  HTML\u89E3\u6790\u529F\u80FD\uFF0C\u4F46\u4F60\u53EF\u4EE5\u5229\u7528Java\u5E93\
  \u6216Clojure\u7684\u5C01\u88C5\u5668\uFF0C\u4F8B\u5982`enlive`\u6216`hickory`\u3002\
  \u4EE5\u4E0B\u662F\u5982\u4F55\u4F7F\u7528\u8FD9\u4E24\u8005\u7684\u65B9\u6CD5\uFF1A\
  ."
lastmod: '2024-04-05T22:38:46.481765-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A Clojure\u6CA1\u6709\u5185\u7F6E\u7684HTML\u89E3\
  \u6790\u529F\u80FD\uFF0C\u4F46\u4F60\u53EF\u4EE5\u5229\u7528Java\u5E93\u6216Clojure\u7684\
  \u5C01\u88C5\u5668\uFF0C\u4F8B\u5982`enlive`\u6216`hickory`\u3002\u4EE5\u4E0B\u662F\
  \u5982\u4F55\u4F7F\u7528\u8FD9\u4E24\u8005\u7684\u65B9\u6CD5\uFF1A."
title: "\u89E3\u6790HTML"
weight: 43
---

## 如何操作：
Clojure没有内置的HTML解析功能，但你可以利用Java库或Clojure的封装器，例如`enlive`或`hickory`。以下是如何使用这两者的方法：

### 使用Enlive：
Enlive是HTML解析和网页抓取的热门选择。首先，在项目依赖中加入它：

```clojure
[net.cgrand/enlive "1.1.6"]
```

然后，你可以这样解析和导航HTML：

```clojure
(require '[net.cgrand.enlive-html :as html])

(let [doc (html/html-resource (java.net.URL. "http://example.com"))]
  (html/select doc [:div.some-class]))
```

这个代码片段获取一个HTML页面，并选择所有带有`some-class`类的`<div>`元素。

输出可能看起来像：

```clojure
({:tag :div, :attrs {:class "some-class"}, :content ["Here's some content."]})
```

### 使用Hickory：
Hickory提供了一种将HTML解析成更易于在Clojure中处理的格式的方法。将Hickory加入到你的项目依赖中：

```clojure
[hickory "0.7.1"]
```

这里有一个简单的例子：

```clojure
(require '[hickory.core :as hickory]
         '[hickory.select :as select])

;; 将HTML解析成Hickory格式
(let [doc (hickory/parse "<html><body><div id='main'>Hello, world!</div></body></html>")]
  ;; 选择id为'main'的div
  (select/select (select/id "main") doc))
```

这段代码解析了一个简单的HTML字符串，并使用CSS选择器找到ID为`main`的`div`。

示例输出：

```clojure
[{:type :element, :tag :div, :attrs {:id "main"}, :content ["Hello, world!"]}]
```

`enlive`和`hickory`都为Clojure中的HTML解析提供了强大的解决方案，其中`enlive`更侧重于模板化，而`hickory`强调数据转换。
