---
title:                "处理JSON数据"
date:                  2024-01-19
simple_title:         "处理JSON数据"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/lua/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON（JavaScript Object Notation）是数据交换的一种格式，读写都很容易。程序员处理JSON主要用于网络通信和配置文件，因为它跨语言通用，易于人类阅读和机器解析。

## How to:
Lua自身没有内建的JSON处理功能，但我们可以用第三方库比如`cjson`或`dkjson`。这里以`cjson`为例：

```Lua
-- 首先安装cjson库
-- luarocks install lua-cjson

-- 引用cjson库
local cjson = require "cjson"

-- 把Lua表转换成JSON字符串
local lua_table = { name = "Zhang Wei", age = 28, programmer = true }
local json_string = cjson.encode(lua_table)
print(json_string)  -- 输出: {"name":"Zhang Wei","age":28,"programmer":true}

-- 把JSON字符串转换成Lua表
local decoded_table = cjson.decode(json_string)
print(decoded_table.name)  -- 输出: Zhang Wei
```

## Deep Dive
JSON于2001年被发明，目的是替代XML等复杂的数据交换格式。它是由Douglas Crockford推广的。实际上，除了`cjson`，Lua社区也提供其他库，如`dkjson`更注重跨平台支持和纯Lua实现。在选择库时考虑性能、易用性、和兼容性。

## See Also
- Lua `cjson`文档：http://www.kyne.com.au/~mark/software/lua-cjson.php
- `dkjson` on GitHub：https://github.com/LuaDist/dkjson
- JSON官方网站：http://json.org/
