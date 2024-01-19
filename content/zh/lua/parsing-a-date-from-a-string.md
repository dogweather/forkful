---
title:                "从字符串解析日期"
html_title:           "C: 从字符串解析日期"
simple_title:         "从字符串解析日期"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/lua/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# 什么 & 为什么？
解析日期从一个字符串就是分析和转换一个字符串为日期格式。程序员之所以这么做，是因为它可以辨认并操作日期信息，方便数据处理。

# 怎么做？
在Lua中，我们可以使用`os.time()`和`string.gsub()`函数来解析日期。

```Lua
string_to_parse = "20年02月02日"
year, month, day = string_to_parse:match("(%d+)年(%d+)月(%d+)日")
parsed_date = os.time({year=year, month=month, day=day})

print(os.date("%x", parsed_date))
```

这段代码的输出会是：
```Lua
02/02/20
```
# 深入研究
（1）在计算机历史的早期，日期解析是一项挑战性的任务，需要严谨地处理每个字符。今天，Lua等语言具有内建函数，可以轻松解析日期。

（2）除了使用`os.time()`和`string.gsub()`之外，Lua也有许多强大的库（如luadate, penlight）可以用来解析日期。

（3）在Lua中，`os.time()`函数返回一个表示日期和时间的数字，而 `string.gsub()`用于执行实际的解析工作，它返回一个新字符串，其中有些字符被其它字符替代。

# 另请参阅
1. Lua Manual: [os.time()](https://www.lua.org/manual/5.3/manual.html#pdf-os.time)
2. Lua Manual: [string.gsub()](https://www.lua.org/manual/5.3/manual.html#pdf-string.gsub)
3. Lua日期库：[luadate](https://github.com/Tieske/date)
4. Lua日常实用库：[penlight](https://github.com/stevedonovan/penlight)