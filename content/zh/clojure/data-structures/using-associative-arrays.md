---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:50.453708-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728 Clojure \u4E2D\uFF0C\u521B\u5EFA\
  \u548C\u64CD\u4F5C\u5173\u8054\u6570\u7EC4\uFF08\u54C8\u5E0C\u6620\u5C04\uFF09\u975E\
  \u5E38\u76F4\u63A5\u3002\u8BA9\u6211\u4EEC\u901A\u8FC7\u793A\u4F8B\u6765\u6DF1\u5165\
  \u4E86\u89E3\u3002 \u521B\u5EFA\u54C8\u5E0C\u6620\u5C04\uFF1A."
lastmod: '2024-04-05T22:38:46.476274-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728 Clojure \u4E2D\uFF0C\u521B\u5EFA\u548C\
  \u64CD\u4F5C\u5173\u8054\u6570\u7EC4\uFF08\u54C8\u5E0C\u6620\u5C04\uFF09\u975E\u5E38\
  \u76F4\u63A5\u3002\u8BA9\u6211\u4EEC\u901A\u8FC7\u793A\u4F8B\u6765\u6DF1\u5165\u4E86\
  \u89E3\u3002 \u521B\u5EFA\u54C8\u5E0C\u6620\u5C04\uFF1A."
title: "\u4F7F\u7528\u5173\u8054\u6570\u7EC4"
weight: 15
---

## 如何操作：
在 Clojure 中，创建和操作关联数组（哈希映射）非常直接。让我们通过示例来深入了解。

创建哈希映射：

```clojure
(def my-map {:name "Alex" :age 30})
```

可以通过指定其键来检索一个值：

```clojure
(get my-map :name)
;; "Alex"
```
或者，更符合惯用语法，可以把键当作一个函数来使用：

```clojure
(:name my-map)
;; "Alex"
```

添加或更新条目很简单：

```clojure
(def updated-map (assoc my-map :location "New York"))
;; {:name "Alex", :age 30, :location "New York"}

(def incremented-age (update my-map :age inc))
;; {:name "Alex", :age 31}
```

用于移除键的是 `dissoc`：

```clojure
(def removed-age (dissoc my-map :age))
;; {:name "Alex"}
```

遍历映射：

```clojure
(doseq [[k v] my-map] (println k "->" v))
;; :name -> Alex
;; :age -> 30
```

而对于条件访问，如果键存在，`find` 返回一个键值对：

```clojure
(find my-map :age)
;; [:age 30]
```

## 深入探索
在 Clojure 中的关联数组，也常被称为哈希映射，对于管理基于键值的数据来说非常灵活和高效。它们是 Clojure 丰富的集合库的一部分，深深植根于该语言不可变性和函数式编程的哲学之中。与数组或列表相比，后者需要 O(n) 的时间复杂度来访问元素，哈希映射提供了几乎恒定的时间复杂度访问，使它们对于查找操作来说非常高效。

有人可能会争论，通过索引访问，Clojure 中的向量可能会服务于类似的目的，但哈希映射在处理非顺序和标签化的数据时表现更为出色，其中键提供了有意义的描述符，而不是一个任意的索引。

独特于 Clojure（及其 Lisp 传承），关联数组是一级公民，这意味着它们可以被直接操作、在函数间传递等，而无需特殊的语法或访问方法。这一设计决策强化了 Clojure 强调的简易性和力量。

虽然哈希映射非常有用，但值得一提的是，对于非常大的数据集或键高度动态（频繁增减）的场景，替代的数据结构或数据库可能提供更好的性能和灵活性。然而，对于 Clojure 应用程序内的大多数典型用例来说，关联数组提供了一种健壮和高效的数据管理手段。
