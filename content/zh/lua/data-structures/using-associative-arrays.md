---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:12:04.889960-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728Lua\u4E2D\uFF0C\u521B\u5EFA\u4E00\
  \u4E2A\u5173\u8054\u6570\u7EC4\uFF08\u6216\u8005\u7528Lua\u7684\u8BDD\u8BF4\u662F\
  \u4E00\u4E2A\u8868\uFF09\u662F\u76F4\u63A5\u4E86\u5F53\u7684\u3002\u4F60\u653E\u5F03\
  \u4E86\u901A\u5E38\u7684\u6570\u503C\u7D22\u5F15\uFF0C\u8F6C\u800C\u9009\u62E9\u4F60\
  \u81EA\u5DF1\u7684\u952E\u3002\u770B\u770B\u8FD9\u4E2A\uFF1A."
lastmod: '2024-04-05T21:53:48.208909-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u4F7F\u7528\u5173\u8054\u6570\u7EC4"
weight: 15
---

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
