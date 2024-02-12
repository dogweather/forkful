---
title:                "使用关联数组"
aliases:
- /zh/python/using-associative-arrays/
date:                  2024-01-30T19:12:49.189990-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用关联数组"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么？

在Python中，关联数组被称为字典，它将键映射到值，使得通过唯一标识符检索、修改或跟踪数据变得容易。程序员之所以使用它们，是因为它们在访问元素和表示复杂数据结构方面的效率和灵活性。

## 如何操作：

在Python中创建字典非常直接。您需要将键值对用大括号`{}`括起来，键和值之间用冒号分隔：

```Python
# 创建一个关联数组（字典）
my_dict = {"name": "John", "age": 30, "city": "New York"}
print(my_dict)
```

输出：
```
{'name': 'John', 'age': 30, 'city': 'New York'}
```

通过键访问一个值很简单：

```Python
# 访问一个值
print(my_dict["name"])
```

输出：
```
John
```

添加或更新元素是通过给一个键分配一个值完成的：

```Python
# 添加一个新的键值对
my_dict["email"] = "john@example.com"
# 更新一个值
my_dict["age"] = 31
print(my_dict)
```

输出：
```
{'name': 'John', 'age': 31, 'city': 'New York', 'email': 'john@example.com'}
```

迭代字典项：

```Python
# 遍历键值对
for key, value in my_dict.items():
    print(f"{key}: {value}")
```

输出：
```
name: John
age: 31
city: New York
email: john@example.com
```

## 深入探索

Python中的关联数组或字典，是为了提供一个高效数据访问和操作的数据结构而引入的。与通过一系列数字索引的序列不同，字典是通过键来索引的，这些键可以是任何不可变类型。这种设计选择使得字典非常适合用作快速查找表，在这里键映射到唯一值。

从历史上看，Python的字典是使用哈希表实现的，确保了查找、插入和删除操作的平均时间复杂度为O(1)。在Python 3.6及以后版本中，字典还维护了项的插入顺序，结合了哈希表的优势和有序数据结构中见到的插入顺序的可预测性。

虽然字典非常灵活，但在一些特殊情况下，`collections.defaultdict`或`collections.OrderedDict`（在Python 3.7之前）等替代品可能更合适。`defaultdict`在需要一个字典为不存在的键返回一个默认值时特别有用，简化了某些类型的条件逻辑。然而，随着Python的持续改进和发展，内置的字典类往往仍是关联数组的首选，因为其稳健性和开箱即用所提供的便利。
