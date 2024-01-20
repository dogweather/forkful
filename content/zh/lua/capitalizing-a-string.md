---
title:                "将字符串大写"
html_title:           "Lua: 将字符串大写"
simple_title:         "将字符串大写"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/lua/capitalizing-a-string.md"
---

{{< edit_this_page >}}

### 什么 & 为什么?
将一个字符串进行大写处理，就是将所有文本字符转为大写。程序员经常需要用到这个技术来规范字符串的显示或者进行某些特定的字符串操作。

### 如何实现
在 Lua 中，我们可以使用 `string.upper` 函数来实现。这是一个简单的示例:

```Lua
str = "hello world from lua"
print(string.upper(str))
```

输出是：

```
HELLO WORLD FROM LUA
```

### 深入探讨
1. 历史背景: 实际上，字符串大写的概念可以追溯到 ASCII 码的出现，当时人们定义了标准的大写和小写字符。

2. 可选策略: Lua 中还存在另一种方式来大写字符串，使用 `string.gsub` 函数和 `string.char` 函数。但这种方式比 `string.upper` 函数要复杂一些。

```Lua
s = "hello world from lua"
s = string.gsub(s, "%a", function(c)
    return string.format("%c", string.byte(c) - 32)
end)
print(s)
```

3. 内部实现: 在 `string.upper` 的内部实现中，Lua 使用了当前语言环境中定义的大写映射。这也意味着 `string.upper` 的行为可能会受到语言环境设置的影响。

### 另见
- [Lua 5.4 参考手册](https://www.lua.org/manual/5.4/manual.html#6.4)
- [Lua 用户手册和字符串库](https://www.lua.org/manual/5.4/manual.html#6.4.1)