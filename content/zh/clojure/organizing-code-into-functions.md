---
title:                "将代码组织成函数"
date:                  2024-01-26T01:09:29.718840-07:00
model:                 gpt-4-1106-preview
simple_title:         "将代码组织成函数"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/organizing-code-into-functions.md"
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
