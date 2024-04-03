---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:12:24.706825-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Java \u6CA1\u6709\u50CF\u67D0\u4E9B\u8BED\
  \u8A00\u90A3\u6837\u5185\u7F6E\u7684\u5173\u8054\u6570\u7EC4\uFF0C\u4F46\u5B83\u63D0\
  \u4F9B\u4E86`Map`\u63A5\u53E3\u548C`HashMap`\u3001`TreeMap`\u7B49\u7C7B\u6765\u586B\
  \u8865\u8FD9\u4E00\u89D2\u8272\u3002\u4EE5\u4E0B\u662F\u5982\u4F55\u4F7F\u7528`HashMap`\u7684\
  \u65B9\u6CD5\uFF1A."
lastmod: '2024-03-13T22:44:47.617687-06:00'
model: gpt-4-0125-preview
summary: "Java \u6CA1\u6709\u50CF\u67D0\u4E9B\u8BED\u8A00\u90A3\u6837\u5185\u7F6E\u7684\
  \u5173\u8054\u6570\u7EC4\uFF0C\u4F46\u5B83\u63D0\u4F9B\u4E86`Map`\u63A5\u53E3\u548C\
  `HashMap`\u3001`TreeMap`\u7B49\u7C7B\u6765\u586B\u8865\u8FD9\u4E00\u89D2\u8272\u3002\
  \u4EE5\u4E0B\u662F\u5982\u4F55\u4F7F\u7528`HashMap`\u7684\u65B9\u6CD5\uFF1A."
title: "\u4F7F\u7528\u5173\u8054\u6570\u7EC4"
weight: 15
---

## 如何操作：
Java 没有像某些语言那样内置的关联数组，但它提供了`Map`接口和`HashMap`、`TreeMap`等类来填补这一角色。以下是如何使用`HashMap`的方法：

```Java
import java.util.HashMap;
import java.util.Map;

public class LearnMaps {
    public static void main(String[] args) {
        // 创建一个 HashMap
        Map<String, Integer> ageOfFriends = new HashMap<>();
        
        // 添加元素
        ageOfFriends.put("Alice", 24);
        ageOfFriends.put("Bob", 30);
        ageOfFriends.put("Charlie", 28);

        // 访问元素
        System.out.println("Alice 的年龄: " + ageOfFriends.get("Alice"));
        
        // 处理不存在的键
        System.out.println("不在映射中的某人的年龄: " + ageOfFriends.getOrDefault("Dan", -1));

        // 迭代元素
        for (Map.Entry<String, Integer> entry : ageOfFriends.entrySet()) {
            System.out.println(entry.getKey() + " 今年 " + entry.getValue() + " 岁。");
        }
    }
}
```

示例输出：

```
Alice 的年龄: 24
不在映射中的某人的年龄: -1
Alice 今年 24 岁。
Bob 今年 30 岁。
Charlie 今年 28 岁。
```

`HashMap` 只是一种实现。如果你的键是唯一的并且你需要它们排序，可以考虑使用 `TreeMap`。对于保留插入顺序的映射，`LinkedHashMap` 是你的好朋友。

## 深入了解
Java 中的映射是集合框架的一部分，最初在 JDK 1.2 中引入，但多年来已经有了显著的改进，包括在 Java 8 中引入的 `forEach` 方法，以更容易地迭代映射条目。映射实现的选择（`HashMap`、`LinkedHashMap`、`TreeMap`）应由你的特定需求决定，这些需求涉及排序和性能。例如，`HashMap` 提供 O(1) 时间性能用于基本操作（get 和 put），假设哈希函数能够在桶之间正确地分散元素。然而，如果你需要基于自然排序或自定义比较器进行排序，`TreeMap` 是首选，提供 O(log n) 时间用于插入和查找。

在引入 `Map` 之前，关联数组通常是通过两个并行数组（一个用于键，一个用于值）或自定义数据结构以较低的效率来实现的。`Map` 及其实现的当前替代品可能包括第三方库提供的专门映射，例如双向映射（Google 的 Guava 库中的 BiMap），用于需要高效地通过值找到键的情况。然而，对于 Java 中的大多数用例，标准库的映射足够健壮和灵活，能够处理任务。
