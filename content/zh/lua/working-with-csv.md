---
title:                "处理 CSV 文件"
html_title:           "Bash: 处理 CSV 文件"
simple_title:         "处理 CSV 文件"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/lua/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? 什么是CSV以及为什么使用？
CSV（逗号分隔值）是一种简单文件格式，用于存储表格数据。程序员使用CSV因为它易于阅读和写入，兼容性好且由许多编程语言和应用程序支持。

## How to: 如何操作
```Lua
-- 引入CSV文件处理库
local csv = require("csv")

-- 读取CSV文件
local file = csv.open("example.csv")
for fields in file:lines() do
    for i, v in ipairs(fields) do
        print(v) -- 输出各字段值
    end
end

-- 写入CSV文件
local data = {
    {"姓名", "年龄", "城市"},
    {"张三", 28, "北京"},
    {"李四", 35, "上海"}
}

local out_file = io.open("output.csv", "w")
for _, row in ipairs(data) do
    out_file:write(table.concat(row, ","), "\n")
end
out_file:close()
```
示例输出：
```
张三
28
北京
...
```

## Deep Dive 深入了解
CSV格式起源于早期电脑，用于简化数据输入输出。尽管现在有如JSON、XML等现代格式，但CSV因其简洁性和跨平台性仍被广泛使用。实现CSV解析通常需要处理转义字符和多行记录。Lua没有内置的CSV处理功能，但可以通过第三方库轻松实现。

## See Also 相关资源
- [Programming in Lua](https://www.lua.org/pil/): 官方Lua编程手册。
- [Lua CSV modules](https://luarocks.org/search?q=csv): 在LuaRocks上的CSV模块。
- [RFC 4180](https://tools.ietf.org/html/rfc4180): CSV文件的标准规范。
