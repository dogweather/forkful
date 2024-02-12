---
title:                "写入标准错误"
aliases: - /zh/lua/writing-to-standard-error.md
date:                  2024-02-03T19:33:48.565840-07:00
model:                 gpt-4-0125-preview
simple_title:         "写入标准错误"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/lua/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么？
将错误信息和诊断输出写入标准错误（stderr）是指将这些信息定向到一个与标准输出（stdout）不同的独立通道。程序员这样做是为了将常规程序结果和错误信息区分开来，从而简化调试和日志记录过程。

## 如何操作：
在Lua中，可以通过使用`io.stderr:write()`函数来实现向stderr写入内容。以下是如何向标准错误写入一个简单错误消息的方法：

```lua
io.stderr:write("错误：输入无效。\n")
```

如果您需要输出一个变量或组合多个数据片段，可以在write函数内部连接它们：

```lua
local errorMessage = "输入无效。"
io.stderr:write("错误：" .. errorMessage .. "\n")
```

**stderr 上的示例输出：**
```
错误：输入无效。
```

对于更复杂的场景，或在处理较大的应用程序时，您可能会考虑使用第三方日志库，如LuaLogging。通过LuaLogging，您可以将日志定向到不同的目的地，包括stderr。这里有一个简短的例子：

首先，使用LuaRocks确保LuaLogging已安装：

```
luarocks install lualogging
```

然后，使用LuaLogging向stderr写入一条错误消息：

```lua
local logging = require("logging")
local logger = logging.stderr()
logger:error("错误：输入无效。")
```

这种方法提供了在您的应用程序中进行标准化日志记录的优势，同时通过一个简单的API增加了设置日志级别（例如，错误、警告、信息）的灵活性。
