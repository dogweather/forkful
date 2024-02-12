---
title:                "解析HTML"
aliases:
- /zh/clojure/parsing-html/
date:                  2024-02-03T19:11:46.605276-07:00
model:                 gpt-4-0125-preview
simple_title:         "解析HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么以及为什么？

在Clojure中解析HTML涉及到从HTML文档中以编程方式提取信息。程序员这么做是为了动态访问、操作或监控网页内容，自动化任务或将数据输入到应用程序中。

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
