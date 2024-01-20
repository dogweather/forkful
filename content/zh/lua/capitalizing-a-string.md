---
title:                "字符串首字母大写"
html_title:           "Arduino: 字符串首字母大写"
simple_title:         "字符串首字母大写"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/lua/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (什么和为什么？)
将字符串首字母大写可以改善文本的可读性，用于标题或用户的名字。程序员这么做以满足语法规则或用户界面的设计需求。

## How to: (如何操作：)
Lua没有内建的首字母大写函数，但我们可以自己写一个。下面是简单例子及输出：

```Lua
function capitalize(str)
    return (str:gsub("^%l", string.upper))
end

print(capitalize("hello")) -- 输出: Hello
```

## Deep Dive (深入探索)
Lua语言始终以简单和轻量著称，不像其他语言（比如Python或Java），它并不提供内建的字符串首字母大写功能。程序员需要利用模式匹配（`%l`匹配小写字母）和`string.upper`函数来实现。除了上面提供的简单函数，你还可以用其他Lua字符串函数搭配使用，比如`string.gsub`，来处理更复杂的情况。不过，Lua社区有提供一些现成的字符串操作库如Penlight，其中包含了更多字符串处理函数。

## See Also (另请参阅)
- Lua 5.4参考手册: https://www.lua.org/manual/5.4/
- Lua字符串模式匹配文档: https://www.lua.org/pil/20.2.html
- Penlight Lua Libraries: https://github.com/lunarmodules/Penlight