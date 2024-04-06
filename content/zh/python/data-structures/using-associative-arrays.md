---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:12:49.189990-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728Python\u4E2D\u521B\u5EFA\u5B57\u5178\
  \u975E\u5E38\u76F4\u63A5\u3002\u60A8\u9700\u8981\u5C06\u952E\u503C\u5BF9\u7528\u5927\
  \u62EC\u53F7`{}`\u62EC\u8D77\u6765\uFF0C\u952E\u548C\u503C\u4E4B\u95F4\u7528\u5192\
  \u53F7\u5206\u9694\uFF1A."
lastmod: '2024-04-05T22:38:46.428481-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728Python\u4E2D\u521B\u5EFA\u5B57\u5178\
  \u975E\u5E38\u76F4\u63A5\u3002\u60A8\u9700\u8981\u5C06\u952E\u503C\u5BF9\u7528\u5927\
  \u62EC\u53F7`{}`\u62EC\u8D77\u6765\uFF0C\u952E\u548C\u503C\u4E4B\u95F4\u7528\u5192\
  \u53F7\u5206\u9694\uFF1A."
title: "\u4F7F\u7528\u5173\u8054\u6570\u7EC4"
weight: 15
---

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
