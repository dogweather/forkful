---
title:                "处理XML"
date:                  2024-01-26T04:33:41.021611-07:00
model:                 gpt-4-0125-preview
simple_title:         "处理XML"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/lua/working-with-xml.md"
---

{{< edit_this_page >}}

## 什么和为什么？
使用 XML 涉及使用代码解析和操作 XML 文档。程序员这样做是为了以一种结构化、可移植的格式读取、写入和修改数据，这种格式广泛用于数据交换和存储。

## 如何操作：
Lua 并没有包含原生的 XML 解析功能，但是有些库，比如 LuaXML 和 xml2lua，可以完成这项工作。这里是使用 xml2lua 解析 XML 的快速入门：

```Lua
local xml2lua = require("xml2lua")
local handler = require("xmlhandler.tree")

local xmlParser = xml2lua.parser(handler)
xmlParser:parse([[<root><book id="123">Programming in Lua</book></root>]])

print(handler.root.book._attr.id)  -- 输出: 123
print(handler.root.book[1])        -- 输出: Programming in Lua
```

对于写 XML，这里有一个使用 LuaXML 的小示例：

```Lua
local luaxml = require("LuaXML")

local xml = xml.new("root")
xml:append("book")[1] = "Programming in Lua"
xml.book._attr = {id="123"}

print(xml:tag())  -- 输出: <root><book id="123">Programming in Lua</book></root>
```

## 深入探讨
XML，即可扩展标记语言，自 90 年代中期以来一直是数据表示和交换的标准。它为数据提供了结构，既易于人类阅读，也能被机器解析。

虽然 JSON 和 YAML 现在因其简易性而更受青睐，但在许多企业和遗留系统中，XML 仍然普遍存在。因为 Lua 设计上旨在通过模块保持小巧和可扩展性，Lua 中没有内置的 XML 处理功能。

Lua 的 XML 库，比如 LuaXML、xml2lua 及其他，弥补了这一差距。LuaXML 提供了轻量级的 XML 读写器，而 xml2lua 使用了类似于 SAX 解析器的事件驱动方法。这些库通常都是用纯 Lua 实现，以便于移植，尽管有些可能会依赖 C 语言以提升性能。

当谈到性能和内存使用时，Lua 的 XML 库可能不会像那些具有原生支持的语言那样快。然而，对于 Lua 中的大多数用例，特别是在游戏开发或为嵌入式系统编写脚本时，这些库在不过度负载系统的情况下都完成得很好。

## 另请参见
- LuaXML 在 GitHub 上: https://github.com/LuaDist/luaxml
- xml2lua 在 GitHub 上: https://github.com/manoelcampos/xml2lua
- Lua.org 的库列表: https://lua-users.org/wiki/LibrariesAndBindings