---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:12:49.189990-07:00
description: "\u5728Python\u4E2D\uFF0C\u5173\u8054\u6570\u7EC4\u88AB\u79F0\u4E3A\u5B57\
  \u5178\uFF0C\u5B83\u5C06\u952E\u6620\u5C04\u5230\u503C\uFF0C\u4F7F\u5F97\u901A\u8FC7\
  \u552F\u4E00\u6807\u8BC6\u7B26\u68C0\u7D22\u3001\u4FEE\u6539\u6216\u8DDF\u8E2A\u6570\
  \u636E\u53D8\u5F97\u5BB9\u6613\u3002\u7A0B\u5E8F\u5458\u4E4B\u6240\u4EE5\u4F7F\u7528\
  \u5B83\u4EEC\uFF0C\u662F\u56E0\u4E3A\u5B83\u4EEC\u5728\u8BBF\u95EE\u5143\u7D20\u548C\
  \u8868\u793A\u590D\u6742\u6570\u636E\u7ED3\u6784\u65B9\u9762\u7684\u6548\u7387\u548C\
  \u7075\u6D3B\u6027\u3002"
lastmod: '2024-03-13T22:44:47.247200-06:00'
model: gpt-4-0125-preview
summary: "\u5728Python\u4E2D\uFF0C\u5173\u8054\u6570\u7EC4\u88AB\u79F0\u4E3A\u5B57\
  \u5178\uFF0C\u5B83\u5C06\u952E\u6620\u5C04\u5230\u503C\uFF0C\u4F7F\u5F97\u901A\u8FC7\
  \u552F\u4E00\u6807\u8BC6\u7B26\u68C0\u7D22\u3001\u4FEE\u6539\u6216\u8DDF\u8E2A\u6570\
  \u636E\u53D8\u5F97\u5BB9\u6613\u3002\u7A0B\u5E8F\u5458\u4E4B\u6240\u4EE5\u4F7F\u7528\
  \u5B83\u4EEC\uFF0C\u662F\u56E0\u4E3A\u5B83\u4EEC\u5728\u8BBF\u95EE\u5143\u7D20\u548C\
  \u8868\u793A\u590D\u6742\u6570\u636E\u7ED3\u6784\u65B9\u9762\u7684\u6548\u7387\u548C\
  \u7075\u6D3B\u6027\u3002."
title: "\u4F7F\u7528\u5173\u8054\u6570\u7EC4"
weight: 15
---

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
