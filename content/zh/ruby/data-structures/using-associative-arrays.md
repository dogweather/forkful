---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:12:51.268941-07:00
description: "\u5173\u8054\u6570\u7EC4\uFF0C\u5728Ruby\u4E2D\u66F4\u5E38\u88AB\u79F0\
  \u4E3A\u6563\u5217(hash)\uFF0C\u5141\u8BB8\u5C06\u552F\u4E00\u7684\u952E\u4E0E\u503C\
  \u914D\u5BF9\u3002\u5F53\u4F60\u9700\u8981\u901A\u8FC7\u7279\u5B9A\u7684\u53C2\u8003\
  \u6765\u8DDF\u8E2A\u5143\u7D20\uFF0C\u5982\u5B58\u50A8\u5BF9\u8C61\u7684\u5C5E\u6027\
  \u6216\u901A\u8FC7\u552F\u4E00\u6807\u8BC6\u7B26\u5FEB\u901F\u8BBF\u95EE\u6570\u636E\
  \u65F6\uFF0C\u5B83\u4EEC\u662F\u4E0D\u53EF\u6216\u7F3A\u7684\u3002"
lastmod: '2024-02-25T18:49:45.921486-07:00'
model: gpt-4-0125-preview
summary: "\u5173\u8054\u6570\u7EC4\uFF0C\u5728Ruby\u4E2D\u66F4\u5E38\u88AB\u79F0\u4E3A\
  \u6563\u5217(hash)\uFF0C\u5141\u8BB8\u5C06\u552F\u4E00\u7684\u952E\u4E0E\u503C\u914D\
  \u5BF9\u3002\u5F53\u4F60\u9700\u8981\u901A\u8FC7\u7279\u5B9A\u7684\u53C2\u8003\u6765\
  \u8DDF\u8E2A\u5143\u7D20\uFF0C\u5982\u5B58\u50A8\u5BF9\u8C61\u7684\u5C5E\u6027\u6216\
  \u901A\u8FC7\u552F\u4E00\u6807\u8BC6\u7B26\u5FEB\u901F\u8BBF\u95EE\u6570\u636E\u65F6\
  \uFF0C\u5B83\u4EEC\u662F\u4E0D\u53EF\u6216\u7F3A\u7684\u3002"
title: "\u4F7F\u7528\u5173\u8054\u6570\u7EC4"
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
