---
title:                "使用关联数组"
aliases:
- /zh/cpp/using-associative-arrays.md
date:                  2024-01-30T19:10:21.466650-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用关联数组"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么？

在 C++ 中，关联数组被称为 `std::map` 或 `std::unordered_map`，它们弥补了数组索引和现实世界数据之间的差距，让你可以使用有意义的键。当你需要使用键而不是索引位置来快速查找、插入和删除时，它们是首选。

## 如何使用：

在 C++ 中，通过 `<map>` 和 `<unordered_map>` 头文件，关联数组得以实现。让我们通过示例来看看它们的使用。

### 使用 `std::map`

`std::map` 根据键对元素进行排序。下面是开始使用它的方法：

```C++
#include <iostream>
#include <map>
#include <string>

int main() {
    std::map<std::string, int> ageMap;
    
    // 插入值
    ageMap["Alice"] = 30;
    ageMap["Bob"] = 25;
    
    // 访问值
    std::cout << "Bob 的年龄: " << ageMap["Bob"] << std::endl;
    
    // 遍历 map
    for(const auto &pair : ageMap) {
        std::cout << pair.first << " 的年龄是 " << pair.second << " 岁。" << std::endl;
    }
    
    return 0;
}
```

### 使用 `std::unordered_map`

当顺序不重要但性能重要时，`std::unordered_map` 是你的好朋友，它为插入、查找和删除提供了更快的平均复杂度。

```C++
#include <iostream>
#include <unordered_map>
#include <string>

int main() {
    std::unordered_map<std::string, double> productPrice;
    
    // 插入值
    productPrice["milk"] = 2.99;
    productPrice["bread"] = 1.99;
    
    // 访问值
    std::cout << "牛奶价格: $" << productPrice["milk"] << std::endl;
    
    // 遍历一个 unordered_map
    for(const auto &pair : productPrice) {
        std::cout << pair.first << " 的价格是 $" << pair.second << std::endl;
    }
    
    return 0;
}
```

## 深入了解

C++ 中的关联数组，特别是 `std::map` 和 `std::unordered_map`，不仅仅是关于存储元素。它们为更复杂的数据管理提供了基础，允许像搜索、插入和删除这样的操作在有效的时间复杂度（对于 `std::map` 是对数时间，对于 `std::unordered_map` 是平均情况下的常数时间）下执行。这种效率来自于底层数据结构：对于 `std::map` 是平衡树，对于 `std::unordered_map` 是哈希表。

在这些成为标准库的一部分之前，程序员不得不实现他们自己的版本或使用第三方库，导致了不一致性和潜在的低效率。将映射包含在 C++ 的标准库中，不仅使它们的使用标准化，而且还针对不同的编译器和平台优化了它们的性能。

虽然两者都很强大，但在 `std::map` 和 `std::unordered_map` 之间的选择取决于你的具体使用场景。需要有序数据且不介意轻微的性能折衷吗？选择 `std::map`。如果你追求速度且不关心顺序，`std::unordered_map` 很可能是更好的选择。

然而，重要的是要注意，在处理复杂的数据结构时，总是有权衡。在一些特殊情况下，其他数据结构或甚至第三方库可能提供更好的性能或适合你特定需求的功能。始终基于项目的要求来权衡你的选项。
