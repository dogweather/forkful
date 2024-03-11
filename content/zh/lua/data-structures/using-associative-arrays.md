---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:12:04.889960-07:00
description: "\u5728Lua\u4E2D\uFF0C\u5173\u8054\u6570\u7EC4\u5C31\u50CF\u662F\u6570\
  \u636E\u7684\u79D8\u5BC6\u63E1\u624B\u2014\u2014\u4E0E\u5176\u8BF4\u662F\u7D22\u5F15\
  \u6574\u9F50\u6392\u5217\u7684\u6570\u5B57\uFF0C\u4E0D\u5982\u8BF4\u4F60\u7684\u952E\
  \u53EF\u4EE5\u968F\u4F60\u6240\u6B32\uFF0C\u8FD9\u4F7F\u5F97\u6570\u636E\u68C0\u7D22\
  \u53D8\u5F97\u8F7B\u800C\u6613\u4E3E\u3002\u7A0B\u5E8F\u5458\u4E3A\u4EC0\u4E48\u8981\
  \u7528\u5B83\u4EEC\u5462\uFF1F\u56E0\u4E3A\u6709\u65F6\u5019\uFF0C\u4F60\u9700\u8981\
  \u901A\u8FC7\u540D\u79F0\u800C\u4E0D\u662F\u6392\u5217\u53F7\u6765\u8C03\u7528\u4E00\
  \u4E2A\u6570\u636E\u3002"
lastmod: '2024-03-11T00:14:21.696044-06:00'
model: gpt-4-0125-preview
summary: "\u5728Lua\u4E2D\uFF0C\u5173\u8054\u6570\u7EC4\u5C31\u50CF\u662F\u6570\u636E\
  \u7684\u79D8\u5BC6\u63E1\u624B\u2014\u2014\u4E0E\u5176\u8BF4\u662F\u7D22\u5F15\u6574\
  \u9F50\u6392\u5217\u7684\u6570\u5B57\uFF0C\u4E0D\u5982\u8BF4\u4F60\u7684\u952E\u53EF\
  \u4EE5\u968F\u4F60\u6240\u6B32\uFF0C\u8FD9\u4F7F\u5F97\u6570\u636E\u68C0\u7D22\u53D8\
  \u5F97\u8F7B\u800C\u6613\u4E3E\u3002\u7A0B\u5E8F\u5458\u4E3A\u4EC0\u4E48\u8981\u7528\
  \u5B83\u4EEC\u5462\uFF1F\u56E0\u4E3A\u6709\u65F6\u5019\uFF0C\u4F60\u9700\u8981\u901A\
  \u8FC7\u540D\u79F0\u800C\u4E0D\u662F\u6392\u5217\u53F7\u6765\u8C03\u7528\u4E00\u4E2A\
  \u6570\u636E\u3002"
title: "\u4F7F\u7528\u5173\u8054\u6570\u7EC4"
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
