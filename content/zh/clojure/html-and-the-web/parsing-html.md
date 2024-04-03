---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:46.605276-07:00
description: "\u5728Clojure\u4E2D\u89E3\u6790HTML\u6D89\u53CA\u5230\u4ECEHTML\u6587\
  \u6863\u4E2D\u4EE5\u7F16\u7A0B\u65B9\u5F0F\u63D0\u53D6\u4FE1\u606F\u3002\u7A0B\u5E8F\
  \u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u52A8\u6001\u8BBF\u95EE\u3001\u64CD\u4F5C\
  \u6216\u76D1\u63A7\u7F51\u9875\u5185\u5BB9\uFF0C\u81EA\u52A8\u5316\u4EFB\u52A1\u6216\
  \u5C06\u6570\u636E\u8F93\u5165\u5230\u5E94\u7528\u7A0B\u5E8F\u4E2D\u3002"
lastmod: '2024-03-13T22:44:47.300427-06:00'
model: gpt-4-0125-preview
summary: "\u5728Clojure\u4E2D\u89E3\u6790HTML\u6D89\u53CA\u5230\u4ECEHTML\u6587\u6863\
  \u4E2D\u4EE5\u7F16\u7A0B\u65B9\u5F0F\u63D0\u53D6\u4FE1\u606F\u3002\u7A0B\u5E8F\u5458\
  \u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u52A8\u6001\u8BBF\u95EE\u3001\u64CD\u4F5C\u6216\
  \u76D1\u63A7\u7F51\u9875\u5185\u5BB9\uFF0C\u81EA\u52A8\u5316\u4EFB\u52A1\u6216\u5C06\
  \u6570\u636E\u8F93\u5165\u5230\u5E94\u7528\u7A0B\u5E8F\u4E2D\u3002."
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
