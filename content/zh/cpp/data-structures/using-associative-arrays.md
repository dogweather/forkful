---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:21.466650-07:00
description: "\u5728 C++ \u4E2D\uFF0C\u5173\u8054\u6570\u7EC4\u88AB\u79F0\u4E3A `std::map`\
  \ \u6216 `std::unordered_map`\uFF0C\u5B83\u4EEC\u5F25\u8865\u4E86\u6570\u7EC4\u7D22\
  \u5F15\u548C\u73B0\u5B9E\u4E16\u754C\u6570\u636E\u4E4B\u95F4\u7684\u5DEE\u8DDD\uFF0C\
  \u8BA9\u4F60\u53EF\u4EE5\u4F7F\u7528\u6709\u610F\u4E49\u7684\u952E\u3002\u5F53\u4F60\
  \u9700\u8981\u4F7F\u7528\u952E\u800C\u4E0D\u662F\u7D22\u5F15\u4F4D\u7F6E\u6765\u5FEB\
  \u901F\u67E5\u627E\u3001\u63D2\u5165\u548C\u5220\u9664\u65F6\uFF0C\u5B83\u4EEC\u662F\
  \u9996\u9009\u3002"
lastmod: '2024-03-13T22:44:48.102640-06:00'
model: gpt-4-0125-preview
summary: "\u5728 C++ \u4E2D\uFF0C\u5173\u8054\u6570\u7EC4\u88AB\u79F0\u4E3A `std::map`\
  \ \u6216 `std::unordered_map`\uFF0C\u5B83\u4EEC\u5F25\u8865\u4E86\u6570\u7EC4\u7D22\
  \u5F15\u548C\u73B0\u5B9E\u4E16\u754C\u6570\u636E\u4E4B\u95F4\u7684\u5DEE\u8DDD\uFF0C\
  \u8BA9\u4F60\u53EF\u4EE5\u4F7F\u7528\u6709\u610F\u4E49\u7684\u952E\u3002\u5F53\u4F60\
  \u9700\u8981\u4F7F\u7528\u952E\u800C\u4E0D\u662F\u7D22\u5F15\u4F4D\u7F6E\u6765\u5FEB\
  \u901F\u67E5\u627E\u3001\u63D2\u5165\u548C\u5220\u9664\u65F6\uFF0C\u5B83\u4EEC\u662F\
  \u9996\u9009\u3002."
title: "\u4F7F\u7528\u5173\u8054\u6570\u7EC4"
weight: 15
---

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
