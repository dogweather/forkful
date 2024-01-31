---
title:                "代码重构"
date:                  2024-01-26T01:17:06.982434-07:00
model:                 gpt-4-0125-preview
simple_title:         "代码重构"

category:             "C"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/refactoring.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
重构是在不改变外部行为的前提下重新组织现有计算机代码的过程。程序员进行重构是为了提高可读性、降低复杂性或使代码更易于维护和扩展，这可以在今后节省大量的时间和头痛问题。

## 如何进行：
让我们来优化一些代码。想象你有一个函数，它计算数组中整数的平均值。乍一看，它有点混乱。

**重构前：**
```C
#include <stdio.h>

double calculateStuff(int numbers[], int size) {
    int sum = 0;
    double average;
    for(int i = 0; i < size; sum += numbers[i++]); // 在for循环条件中进行求和，哎哟！
    average = (double)sum / size;
    return average;
}

int main() {
    int array[] = {23, 42, 57, 16, 5, 102};
    int length = sizeof(array) / sizeof(array[0]);
    printf("Average: %f\n", calculateStuff(array, length));

    return 0;
}
```

**重构后：**
```C
#include <stdio.h>

int calculateSum(const int numbers[], int size) {
    int sum = 0;
    for (int i = 0; i < size; ++i) {
        sum += numbers[i];
    }
    return sum;
}

double calculateAverage(const int numbers[], int size) {
    int sum = calculateSum(numbers, size);
    return (double)sum / size;
}

int main() {
    int array[] = {23, 42, 57, 16, 5, 102};
    int length = sizeof(array) / sizeof(array[0]);
    printf("Average: %f\n", calculateAverage(array, length));
    return 0;
}
```
即使是通过这个简单的例子，你也可以看到，拆分函数使得代码更加清晰和易于维护。现在，每个函数都有一个单一的职责——这是干净编码的一个关键原则。

## 深入了解
术语“重构”在90年代末开始流行，特别是随着Martin Fowler的书《重构：改善既有代码的设计》的出版。重构并不意味着修复bug或添加新功能，而是关于改善代码的结构。

有许多优秀的重构工具和集成开发环境(IDEs)，可以帮助自动化这一过程，如用于C和C++的CLion，但理解底层发生的事情仍然至关重要。

重构的替代方案可能包括从头开始重写代码（风险高且通常不必要）或忍受技术债务（从长远来看可能成本更高）。实施细节基于项目而有所不同，但常见的重构包括为了清晰而重命名变量、将大函数拆分成小函数、以及用命名常量替换魔术数字。

此外，DRY（不要重复自己）和SOLID原则等模式可以指导你的重构之旅，推动代码库更易于测试、理解和协作。

## 另见
要深入了解重构的海洋，请查看：

- Martin Fowler的主页：https://martinfowler.com/，提供有关重构和软件设计的大量文章和资源。
- Refactoring.com：https://refactoring.com/，提供重构技术的示例和目录。
- 《重构》这本书：被认为是重构的圣经，阅读它可以让你全面了解该方法论。
- 《代码整洁之道：敏捷软件工艺指南》作者Robert C. Martin，讨论了如何编写易于理解和维护的代码。
