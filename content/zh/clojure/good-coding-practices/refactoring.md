---
date: 2024-01-26 01:17:28.707144-07:00
description: "\u5982\u4F55\u8FDB\u884C\uFF1A \u5728 Clojure \u4E2D\u8FDB\u884C\u91CD\
  \u6784\u2014\u2014\u5F97\u76CA\u4E8E\u5176\u7B80\u6D01\u7684\u8BED\u6CD5\u548C\u51FD\
  \u6570\u5F0F\u8303\u5F0F\u2014\u2014\u53EF\u4EE5\u975E\u5E38\u76F4\u63A5\u3002\u8BA9\
  \u6211\u4EEC\u6765\u5904\u7406\u4E00\u4E2A\u5E38\u89C1\u573A\u666F\uFF1A\u904D\u5386\
  \u96C6\u5408\u3002\u4F60\u53EF\u80FD\u4F1A\u4ECE\u4E00\u4E2A `for` \u5FAA\u73AF\u5F00\
  \u59CB\uFF0C\u50CF\u8FD9\u6837\uFF1A."
lastmod: '2024-03-13T22:44:47.312018-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Clojure \u4E2D\u8FDB\u884C\u91CD\u6784\u2014\u2014\u5F97\u76CA\u4E8E\
  \u5176\u7B80\u6D01\u7684\u8BED\u6CD5\u548C\u51FD\u6570\u5F0F\u8303\u5F0F\u2014\u2014\
  \u53EF\u4EE5\u975E\u5E38\u76F4\u63A5\u3002\u8BA9\u6211\u4EEC\u6765\u5904\u7406\u4E00\
  \u4E2A\u5E38\u89C1\u573A\u666F\uFF1A\u904D\u5386\u96C6\u5408\u3002\u4F60\u53EF\u80FD\
  \u4F1A\u4ECE\u4E00\u4E2A `for` \u5FAA\u73AF\u5F00\u59CB\uFF0C\u50CF\u8FD9\u6837\uFF1A\
  ."
title: "\u4EE3\u7801\u91CD\u6784"
weight: 19
---

## 如何进行：
在 Clojure 中进行重构——得益于其简洁的语法和函数式范式——可以非常直接。让我们来处理一个常见场景：遍历集合。你可能会从一个 `for` 循环开始，像这样：

```clojure
(defn calculate-sum [numbers]
  (reduce + 0 numbers))

(defn old-way []
  (let [nums (range 1 11)]
    (calculate-sum nums)))
```

调用 `(old-way)` 将给我们 55，也就是从 1 到 10 的和。但是，嘿，我们可以重构这个使其更符合 Clojure 风格：

```clojure
(defn new-way []
  (->> (range 1 11)
       (reduce +)))
```

这个重构后的 `(new-way)` 函数使用线程宏直接将范围传递给 `reduce`，去除了多余的部分。

## 深入探索
重构这门艺术在软件开发的早期就已经有所涉及，但真正得到关注是在 Martin Fowler 的开创性书籍《重构：改善既有代码的设计》1999年出版之后。在 Clojure 中，重构往往依赖于函数式编程原则，偏好纯函数和不可变数据结构。

在 Clojure 中手动重构的替代方法可能包括使用如 Cursive 这样的工具，这是一个受欢迎的 IntelliJ IDEA 插件，提供了特定于 Clojure 的自动重构功能。还有 clj-refactor，一个为 Clojure 提供的 Emacs 包，提供了一系列重构函数。

Clojure 中重构的特殊挑战在于如何在一个原则上不可变且无副作用的范式中处理状态和副作用。在重构过程中，谨慎使用原子、引用、代理和瞬态是维持性能和正确性的关键。

## 另请参阅
- Martin Fowler 的《重构：改善既有代码的设计》，了解基础概念。
- [Clojure 文档](https://clojuredocs.org/)，查找 Clojure 代码的具体示例。
- [clj-refactor](https://github.com/clojure-emacs/clj-refactor.el)，在 Emacs 中进行重构自动化。
- [Cursive](https://cursive-ide.com/)，为使用 IntelliJ 的用户提供自动重构帮助。
- [与 Rich Hickey 一起重构](https://www.infoq.com/presentations/Simple-Made-Easy/) - Clojure 的创建者的一次演讲，虽然不是关于重构本身，但提供了对 Clojure 哲学的洞察，这可以指导有效的重构决策。
