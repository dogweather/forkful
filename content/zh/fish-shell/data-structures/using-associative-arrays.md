---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:11:05.473628-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Fish\u672C\u8EAB\u5E76\u4E0D\u50CFBash\
  \ 4+\u90A3\u6837\u539F\u751F\u652F\u6301\u5173\u8054\u6570\u7EC4\uFF0C\u4F46\u60A8\
  \u53EF\u4EE5\u4F7F\u7528\u5217\u8868\u548C\u5B57\u7B26\u4E32\u64CD\u4F5C\u7684\u7EC4\
  \u5408\u6765\u5B9E\u73B0\u7C7B\u4F3C\u7684\u529F\u80FD\u3002\u4EE5\u4E0B\u662F\u6A21\
  \u62DF\u5B83\u4EEC\u7684\u65B9\u5F0F\uFF1A \u9996\u5148\uFF0C\u5206\u522B\u8BBE\u7F6E\
  \u201C\u5173\u8054\u6570\u7EC4\u201D\u5143\u7D20\uFF1A."
lastmod: '2024-04-05T22:38:47.393210-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A Fish\u672C\u8EAB\u5E76\u4E0D\u50CFBash 4+\u90A3\
  \u6837\u539F\u751F\u652F\u6301\u5173\u8054\u6570\u7EC4\uFF0C\u4F46\u60A8\u53EF\u4EE5\
  \u4F7F\u7528\u5217\u8868\u548C\u5B57\u7B26\u4E32\u64CD\u4F5C\u7684\u7EC4\u5408\u6765\
  \u5B9E\u73B0\u7C7B\u4F3C\u7684\u529F\u80FD\u3002\u4EE5\u4E0B\u662F\u6A21\u62DF\u5B83\
  \u4EEC\u7684\u65B9\u5F0F\uFF1A \u9996\u5148\uFF0C\u5206\u522B\u8BBE\u7F6E\u201C\u5173\
  \u8054\u6570\u7EC4\u201D\u5143\u7D20\uFF1A."
title: "\u4F7F\u7528\u5173\u8054\u6570\u7EC4"
weight: 15
---

## 如何操作：
Fish本身并不像Bash 4+那样原生支持关联数组，但您可以使用列表和字符串操作的组合来实现类似的功能。以下是模拟它们的方式：

首先，分别设置“关联数组”元素：

```Fish Shell
set food_color_apple "red"
set food_color_banana "yellow"
```

要访问一个元素，直接引用它即可：

```Fish Shell
echo $food_color_apple
# 输出：red
```

如果您需要遍历它们，使用for循环并考虑命名约定：

```Fish Shell
for food in apple banana
    echo $food_color_$food
end
# 输出：
# red
# yellow
```

对于那些缺少Bash的`${!array[@]}`来获取所有键的人，您可以在一个单独的列表中存储键：

```Fish Shell
set food_keys apple banana

for key in $food_keys
    echo $key '是' $food_color_$key
end
# 输出：
# apple 是 red
# banana 是 yellow
```

## 深入探讨
真正的关联数组，如其他脚本语言中的，尚未成为Fish方法的一部分。所展示的解决方法利用了Fish的字符串操作和列表功能来创建一个伪关联数组结构。虽然它有效，但并不如内置的关联数组支持那样干净或防错。其他shell，如Bash和Zsh，提供了内置的关联数组功能，这导致更直接、可读的代码。然而，Fish的设计理念旨在追求简洁性和用户友好性，可能以牺牲这类功能为代价。这种解决方法满足了大多数需求，但请关注Fish Shell的发展——其开发人员根据社区反馈积极改进和增加功能。
