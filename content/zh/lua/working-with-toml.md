---
title:                "使用TOML"
date:                  2024-01-26T04:24:27.523111-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用TOML"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/lua/working-with-toml.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
使用TOML涉及使用Lua解析和生成TOML（Tom的显而易见的最简语言）数据。程序员使用TOML来编写配置文件，由于其可读性和简单的语法很容易转换为数据结构。

## 如何操作：
首先，确保你的Lua环境有TOML解析器。我们在这个例子中将使用`lua-toml`。

```Lua
local toml = require("toml")

-- 解析TOML字符串
local toml_data = [[
title = "TOML 示例"

[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z
]]

local data = toml.parse(toml_data)
print(data.title) -- "TOML 示例"

-- 生成TOML字符串
local table_data = {
  title = "TOML 示例",
  owner = {
    name = "Tom Preston-Werner",
    dob = os.time({year=1979, month=5, day=27, hour=7, min=32})
  }
}

local toml_string = toml.encode(table_data)
print(toml_string)
```

示例输出：
```
TOML 示例
```

## 深入了解
TOML由Tom Preston-Werner在2013年创建，作为XML和YAML等其他数据序列化语言的替代品，提供了一种更直接的格式来表示配置数据。虽然JSON无处不在，但其语法对于配置文件而言可能较为繁琐。TOML以更清晰的语法脱颖而出，类似于.ini文件，但具有嵌套能力和数据类型。

TOML的替代品包括JSON、YAML和XML。然而，TOML专为配置而设计，并且可以说比YAML更简单，比作配置用途的JSON更可读，也比XML更简洁。

在Lua中实现TOML处理通常需要第三方库。性能和功能可能会有所不同，从基本解析到完全序列化支持都有。当处理大型配置文件或频繁的读写操作时，考虑库的性能和对最新TOML版本的兼容性。

## 另见
- TOML规范：https://toml.io/en/
- `lua-toml`库：https://github.com/jonstoler/lua-toml
- 数据序列化格式比较：https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats