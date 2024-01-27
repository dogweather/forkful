---
title:                "处理 YAML 文件"
date:                  2024-01-19
html_title:           "Bash: 处理 YAML 文件"
simple_title:         "处理 YAML 文件"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/lua/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (是什么以及为什么？)
YAML是一种数据序列化格式，易于人类阅读和与设备间通讯。程序员使用YAML来配置软件、管理数据和容器编排。

## How to: (如何操作：)
Lua没有内建的YAML解析库，但你可以使用`lyaml`等第三方库。以下是一个简单的例子：

```Lua
-- 引入LYAML库
local lyaml = require('lyaml')

-- YAML字符串
local yaml_str = [[
- name: John Doe
  age: 30
- name: Jane Doe
  age: 25
]]

-- 解析YAML字符串
local data = lyaml.load(yaml_str)

-- 输出解析结果
for i, person in ipairs(data) do
  print(person.name .. " is " .. person.age .. " years old.")
end
```

输出样例:

```
John Doe is 30 years old.
Jane Doe is 25 years old.
```

## Deep Dive (深入探讨)
YAML诞生于2001年，原名是"Yet Another Markup Language"，后改为"YAML Ain't Markup Language"来强调它是一个数据中心的格式。YAML的一些替代品包括JSON和XML。在Lua中处理YAML时，你可以选择`lyaml`或`yaml`等库，但是它们或许需要使用LuaRocks等包管理器来安装。

## See Also (相关资料)
- YAML官方网站: [https://yaml.org](https://yaml.org)
- LYAML库: [https://github.com/gvvaughan/lyaml](https://github.com/gvvaughan/lyaml)
- LuaRocks包管理器: [https://luarocks.org/](https://luarocks.org/)
