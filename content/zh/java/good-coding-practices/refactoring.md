---
aliases:
- /zh/java/refactoring/
date: 2024-01-26 01:18:47.637890-07:00
description: "\u91CD\u6784\u662F\u91CD\u65B0\u6784\u5EFA\u73B0\u6709\u8BA1\u7B97\u673A\
  \u4EE3\u7801\u7684\u8FC7\u7A0B\u2014\u2014\u6539\u53D8\u4EE3\u7801\u7684\u6784\u9020\
  \u2014\u2014\u4F46\u4E0D\u6539\u53D8\u5176\u5916\u90E8\u884C\u4E3A\u3002\u7A0B\u5E8F\
  \u5458\u4E4B\u6240\u4EE5\u8FD9\u6837\u505A\uFF0C\u662F\u4E3A\u4E86\u63D0\u9AD8\u8F6F\
  \u4EF6\u7684\u975E\u529F\u80FD\u5C5E\u6027\uFF0C\u589E\u5F3A\u53EF\u8BFB\u6027\uFF0C\
  \u964D\u4F4E\u590D\u6742\u6027\uFF0C\u5E76\u4F7F\u4EE3\u7801\u66F4\u6613\u4E8E\u672A\
  \u6765\u7684\u7EF4\u62A4\u3002"
lastmod: 2024-02-18 23:08:59.028307
model: gpt-4-0125-preview
summary: "\u91CD\u6784\u662F\u91CD\u65B0\u6784\u5EFA\u73B0\u6709\u8BA1\u7B97\u673A\
  \u4EE3\u7801\u7684\u8FC7\u7A0B\u2014\u2014\u6539\u53D8\u4EE3\u7801\u7684\u6784\u9020\
  \u2014\u2014\u4F46\u4E0D\u6539\u53D8\u5176\u5916\u90E8\u884C\u4E3A\u3002\u7A0B\u5E8F\
  \u5458\u4E4B\u6240\u4EE5\u8FD9\u6837\u505A\uFF0C\u662F\u4E3A\u4E86\u63D0\u9AD8\u8F6F\
  \u4EF6\u7684\u975E\u529F\u80FD\u5C5E\u6027\uFF0C\u589E\u5F3A\u53EF\u8BFB\u6027\uFF0C\
  \u964D\u4F4E\u590D\u6742\u6027\uFF0C\u5E76\u4F7F\u4EE3\u7801\u66F4\u6613\u4E8E\u672A\
  \u6765\u7684\u7EF4\u62A4\u3002"
title: "\u4EE3\u7801\u91CD\u6784"
---

{{< edit_this_page >}}

## 什么 & 为什么？
重构是重新构建现有计算机代码的过程——改变代码的构造——但不改变其外部行为。程序员之所以这样做，是为了提高软件的非功能属性，增强可读性，降低复杂性，并使代码更易于未来的维护。

## 如何进行：
让我们来看一个简单的Java类，由于其组织差和缺乏清晰度，非常需要重构。

```java
public class Calculator {
    public int calc(int op1, int op2, String operation) {
        if (operation.equals("add")) {
            return op1 + op2;
        } else if (operation.equals("subtract")) {
            return op1 - op2;
        } // 其他操作...
    }
}
```

重构后，我们得到：

```java
public class Calculator {
    public int add(int operand1, int operand2) {
        return operand1 + operand2;
    }

    public int subtract(int operand1, int operand2) {
        return operand1 - operand2;
    }

    // 其他操作...
}
```

通过重构，我们改进了方法名称和参数以增强可读性，并消除了单个方法中条件分支的需要。每个操作现在都明确说明了其目的。

## 深入了解：
重构起源于Smalltalk社区，它强调代码的可读性和面向对象的设计，但真正在Java世界中起飞是在90年代末和00年代初，特别是在Martin Fowler的开创性著作《重构：改善既有代码的设计》出版之后。

除了重构之外，还有其他选择，比如从头开始重写代码。然而，重构通常更受欢迎，因为它涉及到不会中断应用功能的渐进式更改。

在Java（或任何编程语言）中进行重构的实施细节涉及理解代码异味——代码中更深层次问题的指标。一些异味包括方法过长、类过大、代码重复和过度使用原始类型。通过应用重构模式，如提取方法、移动方法或用查询替换临时变量，开发人员可以系统地解决这些问题，同时确保代码在任何时候都保持功能性。

自动化工具，如IntelliJ IDEA的重构支持，或Eclipse的插件，可以通过自动化重构来帮助流程，例如重命名变量、方法和类，提取方法或变量，以及将方法或类移动到不同的包或命名空间。

## 另见：
- Martin Fowler的《重构：改善既有代码的设计》：https://martinfowler.com/books/refactoring.html
- 在Refactoring.Guru上的重构技巧：https://refactoring.guru/refactoring/techniques
- Eclipse中的自动化重构：https://www.eclipse.org/eclipse/news/4.18/jdt.php
- IntelliJ IDEA的重构特性：https://www.jetbrains.com/idea/features/refactoring.html

这些资源中的每一个都提供了理解重构原则的基础或者可以利用的工具，以将这些原则付诸实践。
