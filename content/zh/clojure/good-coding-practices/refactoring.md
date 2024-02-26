---
date: 2024-01-26 01:17:28.707144-07:00
description: "\u91CD\u6784\u662F\u5728\u4E0D\u6539\u53D8\u73B0\u6709\u8BA1\u7B97\u673A\
  \u4EE3\u7801\u5916\u90E8\u884C\u4E3A\u7684\u524D\u63D0\u4E0B\uFF0C\u5BF9\u5176\u8FDB\
  \u884C\u7ED3\u6784\u91CD\u7EC4\u7684\u8FC7\u7A0B\uFF0C\u65E8\u5728\u6539\u5584\u975E\
  \u529F\u80FD\u5C5E\u6027\u3002\u7A0B\u5E8F\u5458\u8FDB\u884C\u91CD\u6784\uFF0C\u662F\
  \u4E3A\u4E86\u4F7F\u4EE3\u7801\u66F4\u6E05\u6D01\u3001\u66F4\u9AD8\u6548\uFF0C\u5E76\
  \u66F4\u6613\u4E8E\u7EF4\u62A4\uFF0C\u4ECE\u800C\u6709\u6548\u63D0\u9AD8\u8F6F\u4EF6\
  \u7684\u53EF\u8BFB\u6027\u5E76\u51CF\u5C11\u5176\u590D\u6742\u6027\u3002"
lastmod: '2024-02-25T18:49:44.945829-07:00'
model: gpt-4-0125-preview
summary: "\u91CD\u6784\u662F\u5728\u4E0D\u6539\u53D8\u73B0\u6709\u8BA1\u7B97\u673A\
  \u4EE3\u7801\u5916\u90E8\u884C\u4E3A\u7684\u524D\u63D0\u4E0B\uFF0C\u5BF9\u5176\u8FDB\
  \u884C\u7ED3\u6784\u91CD\u7EC4\u7684\u8FC7\u7A0B\uFF0C\u65E8\u5728\u6539\u5584\u975E\
  \u529F\u80FD\u5C5E\u6027\u3002\u7A0B\u5E8F\u5458\u8FDB\u884C\u91CD\u6784\uFF0C\u662F\
  \u4E3A\u4E86\u4F7F\u4EE3\u7801\u66F4\u6E05\u6D01\u3001\u66F4\u9AD8\u6548\uFF0C\u5E76\
  \u66F4\u6613\u4E8E\u7EF4\u62A4\uFF0C\u4ECE\u800C\u6709\u6548\u63D0\u9AD8\u8F6F\u4EF6\
  \u7684\u53EF\u8BFB\u6027\u5E76\u51CF\u5C11\u5176\u590D\u6742\u6027\u3002"
title: "\u4EE3\u7801\u91CD\u6784"
---

{{< edit_this_page >}}

## 什么 & 为什么？

重构是在不改变现有计算机代码外部行为的前提下，对其进行结构重组的过程，旨在改善非功能属性。程序员进行重构，是为了使代码更清洁、更高效，并更易于维护，从而有效提高软件的可读性并减少其复杂性。

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
