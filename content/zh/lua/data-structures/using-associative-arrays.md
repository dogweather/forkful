---
title:                "使用关联数组"
date:                  2024-01-30T19:12:04.889960-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用关联数组"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/lua/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么与为什么？

在Lua中，关联数组就像是数据的秘密握手——与其说是索引整齐排列的数字，不如说你的键可以随你所欲，这使得数据检索变得轻而易举。程序员为什么要用它们呢？因为有时候，你需要通过名称而不是排列号来调用一个数据。

## 如何操作：

在Lua中，创建一个关联数组（或者用Lua的话说是一个表）是直接了当的。你放弃了通常的数值索引，转而选择你自己的键。看看这个：

```Lua
-- 创建一个关联数组
userInfo = {
  name = "Jamie",
  occupation = "Adventurer",
  level = 42
}

-- 访问元素
print(userInfo["name"]) -- 打印 Jamie
print(userInfo.occupation) -- 打印 Adventurer

-- 添加新的键值对
userInfo["hobby"] = "Coding"
userInfo.favLang = "Lua"

-- 遍历关联数组
for key, value in pairs(userInfo) do
  print(key .. ": " .. value)
end
```

输出：
```
Jamie
Adventurer
name: Jamie
occupation: Adventurer
level: 42
hobby: Coding
favLang: Lua
```

酷的部分？你使用对你有意义的键与数据交互，使代码更可读且可维护。

## 深入探索

当Lua出现在舞台上时，它引入了表作为一种包罗万象的数据结构，彻底改变了开发者管理数据的方式。与某些语言中关联数组和数组是不同实体不同，Lua的表既可以作为数组又可以作为关联数组，简化了数据结构的景观。

Lua表的特别之处在于它们的灵活性。然而，这种灵活性可能会带来潜在的性能影响，尤其是在处理大数据集时，使用更专业的数据结构可能更有利于效率。

虽然Lua并没有内置支持更常见的数据结构，如链表或哈希映射，但表结构的适应性意味着如果需要，你可以使用表来实现这些。只要记住：权力越大，责任越大。明智地使用这种灵活性，以保持你的代码的性能和可读性。
