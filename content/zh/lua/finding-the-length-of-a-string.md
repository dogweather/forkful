---
title:                "查找字符串的长度"
html_title:           "Javascript: 查找字符串的长度"
simple_title:         "查找字符串的长度"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/lua/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 什么与为什么？

查找字符串的长度是找出字符串包含多少个字符。程序员做这个是为了我们能正确地处理字符串数据，例如进行特定的算法操作。

## 如何做：

Lua里面查找字符串长度的方法很直接。你只需要使用内建的`string.len()`函数或者写 `#` 在你的字符串前面。让我们看看它们是如何工作的：

```Lua
str = "你好，世界"
print(string.len(str))  -- 输出 12
print(#str)             -- 输出 12
```
请注意，由于Lua对Unicode支持有限，这些方法可能不能正确处理包含复杂字符的字符串。

## 深入探究

1. 历史背景：`string.len()`在Lua的早期版本就存在。`#`被添加到更晚的版本，使其更易读与快速使用。
2. 替代方案：你也可以通过遍历字符串中的所有字符来找到长度，但这样做一般效率低下。
3. 实现细节：在Lua中，string.len()和`#`本质上相同，它们都通过调用底层的`strlen`C函数来找出字符串长度。

## 参见

1. Lua官方文档：[字符串操作](https://www.lua.org/manual/5.4/manual.html#6.4)
2. ["Lua字符串长度的陷井"](https://stackoverflow.com/questions/9722909/lua-string-length-pitfall)：讲解Lua如何计算包含复杂字符的字符串长度的讨论。
3. ["Lua中的字符串"](https://learnxinyminutes.com/docs/zh-cn/lua-cn/)：学习Lua中字符串的其他概念以及操作方法的资源。