---
title:                "使用关联数组"
aliases:
- /zh/ruby/using-associative-arrays.md
date:                  2024-01-30T19:12:51.268941-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用关联数组"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么和为什么?

关联数组，在Ruby中更常被称为散列(hash)，允许将唯一的键与值配对。当你需要通过特定的参考来跟踪元素，如存储对象的属性或通过唯一标识符快速访问数据时，它们是不可或缺的。

## 如何做：

在Ruby中创建和使用散列是直截了当的。你可以初始化一个空的散列，用键值对填充它，通过它们的键访问值等等。以下是如何操作的：

```Ruby
# 创建一个散列
my_hash = { "name" => "John Doe", "age" => 30 }

# 另一种创建散列的方法
another_hash = Hash.new
another_hash["position"] = "Developer"

# 访问散列值
puts my_hash["name"] # 输出：John Doe

# 添加一个新的键值对
my_hash["language"] = "Ruby"
puts my_hash # 输出：{"name"=>"John Doe", "age"=>30, "language"=>"Ruby"}

# 遍历一个散列
my_hash.each do |key, value|
  puts "#{key}: #{value}"
end
# 输出：
# name: John Doe
# age: 30
# language: Ruby
```

你也可以使用符号作为更高效的键：

```Ruby
# 使用符号作为键
symbol_hash = { name: "Jane Doe", age: 22 }
puts symbol_hash[:name] # 输出：Jane Doe
```

## 深入探究：

关联数组的概念并不独特于Ruby；许多语言在各种名称下实现了它们，如Python中的字典或JavaScript中的对象（当作为键值对使用时）。在Ruby的早期阶段，散列在某种程度上较慢，且不太灵活。然而，随着时间的发展，Ruby对散列的实现已经高度优化，特别是对于符号键，使其对于频繁的访问和更新来说非常高效。

Ruby的散列以其语法的易用性和灵活性而脱颖而出 - 你几乎可以使用任何对象类型作为键，尽管符号和字符串最为常见。在内部，Ruby散列使用哈希算法实现，即使元素数量增加，也能平衡速度和内存效率。

虽然散列非常灵活，但它们并不是Ruby中数据存储的终极解决方案。对于有序的集合，数组更合适；对于独特项的集合，Set可能是更好的选择。此外，对于非常复杂的数据结构，创建自定义类可能是明智的。

记住，使用散列与其他数据结构的选择很大程度上取决于特定的用例——散列在快速查找和维护唯一键及其值之间的关联方面表现出色。
