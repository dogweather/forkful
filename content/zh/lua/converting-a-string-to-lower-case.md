---
title:                "将字符串转换为小写"
date:                  2024-01-20T17:38:58.658484-07:00
model:                 gpt-4-1106-preview
simple_title:         "将字符串转换为小写"

category:             "Lua"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/lua/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? | 什么和为什么?
将字符串转换成小写意味着把所有大写字母改成小写字母。程序员这么做主要是为了数据一致性和比较，因为计算机区分字母大小写。

## How to: | 怎么做:
在Lua中，转换字符串到小写很直接。使用`string.lower()`函数：

```Lua
local originalString = "Hello World!"
local lowerCaseString = string.lower(originalString)
print(lowerCaseString)  -- 输出: hello world!
```

## Deep Dive | 深入探讨:
字符串大小写转换功能在很多语言中都是个基础功能。Lua提供的`string.lower()`函数已经存在很长时间了，是字符串处理工具箱中的一部分。如果考虑支持国际化和特殊字符，那么可能需要更复杂的库，比如Lua的`utf8`库。Lua5.3及之后版本提供内置的`utf8`库，使得操作Unicode字符串变得容易。

```Lua
local utf8String = "ÂBçD"
print(utf8.lower(utf8String))  -- 输出: âbçd
```

要注意的是，`string.lower()`不会改变原字符串; 它会返回一个新的小写字符串。

## See Also | 另请参阅:
- Lua 5.4参考手册: [https://www.lua.org/manual/5.4/](https://www.lua.org/manual/5.4/)
