---
date: 2024-01-26 01:09:29.718840-07:00
description: "\u5C06\u4EE3\u7801\u5757\u7EC4\u7EC7\u6210\u51FD\u6570\u662F\u4E3A\u4E86\
  \u5C06\u5B8C\u6210\u7279\u5B9A\u4EFB\u52A1\u7684\u4EE3\u7801\u6253\u5305\u3002\u8FD9\
  \u6837\u505A\u53EF\u4EE5\u4F7F\u60A8\u7684\u4EE3\u7801\u66F4\u6E05\u6670\u3001\u66F4\
  \u6613\u4E8E\u7EF4\u62A4\uFF0C\u5E76\u4E14\u6613\u4E8E\u5176\u4ED6\u5F00\u53D1\u4EBA\
  \u5458\u9605\u8BFB\u3002"
lastmod: '2024-03-13T22:44:47.308546-06:00'
model: gpt-4-1106-preview
summary: "\u5C06\u4EE3\u7801\u5757\u7EC4\u7EC7\u6210\u51FD\u6570\u662F\u4E3A\u4E86\
  \u5C06\u5B8C\u6210\u7279\u5B9A\u4EFB\u52A1\u7684\u4EE3\u7801\u6253\u5305\u3002\u8FD9\
  \u6837\u505A\u53EF\u4EE5\u4F7F\u60A8\u7684\u4EE3\u7801\u66F4\u6E05\u6670\u3001\u66F4\
  \u6613\u4E8E\u7EF4\u62A4\uFF0C\u5E76\u4E14\u6613\u4E8E\u5176\u4ED6\u5F00\u53D1\u4EBA\
  \u5458\u9605\u8BFB\u3002"
title: "\u5C06\u4EE3\u7801\u7EC4\u7EC7\u6210\u51FD\u6570"
---

{{< edit_this_page >}}

## 什么 & 为什么？

将代码块组织成函数是为了将完成特定任务的代码打包。这样做可以使您的代码更清晰、更易于维护，并且易于其他开发人员阅读。

## 如何操作：

Clojure中的函数使用`defn`定义，后跟一个名字、参数和主体。这里有一个快速示例。

```Clojure
(defn greet [name]
  (str "Hello, " name "!"))

(greet "Alex") ; => "Hello, Alex!"
```

现在假设我们要计算矩形的面积。我们不是将其全部混合在一起，而是将其分成两个函数：

```Clojure
(defn area [length width]
  (* length width))

(defn print-area [length width]
  (println "面积是：" (area length width)))

(print-area 3 4) ; => 面积是：12
```

## 深入了解

很早以前，编码人员会将所有逻辑糟糕地堆放在一个块中。看起来真的很糟糕。然后结构化编程出现了，函数成为了一种东西。在Clojure中，每个函数都是第一类的——你可以像处理任何其他值一样随意处理它们。

还有其他选择吗？有些人可能会研究多方法或高阶函数，但这些不过是函数炖菜中的调料。

函数的所有细节：它们在Clojure中是不可变的，这使得副作用混乱不太可能发生。它们重度依赖递归而不是典型的循环，这与该语言的函数式范式很好地融合在了一起。

## 另请参阅

- Clojure自己的指南：https://clojure.org/guides/learn/functions
- 函数式编程基础：https://www.braveclojure.com/core-functions-in-depth/
- Rich Hickey的讲话：https://changelog.com/posts/rich-hickeys-greatest-hits - 关于Clojure哲学的洞见。
