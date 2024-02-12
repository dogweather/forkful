---
title:                "代码重构"
aliases:
- /zh/java/refactoring.md
date:                  2024-01-26T01:18:47.637890-07:00
model:                 gpt-4-0125-preview
simple_title:         "代码重构"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/refactoring.md"
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
