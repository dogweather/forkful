---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:48.565840-07:00
description: "\u5C06\u9519\u8BEF\u4FE1\u606F\u548C\u8BCA\u65AD\u8F93\u51FA\u5199\u5165\
  \u6807\u51C6\u9519\u8BEF\uFF08stderr\uFF09\u662F\u6307\u5C06\u8FD9\u4E9B\u4FE1\u606F\
  \u5B9A\u5411\u5230\u4E00\u4E2A\u4E0E\u6807\u51C6\u8F93\u51FA\uFF08stdout\uFF09\u4E0D\
  \u540C\u7684\u72EC\u7ACB\u901A\u9053\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\
  \u4E3A\u4E86\u5C06\u5E38\u89C4\u7A0B\u5E8F\u7ED3\u679C\u548C\u9519\u8BEF\u4FE1\u606F\
  \u533A\u5206\u5F00\u6765\uFF0C\u4ECE\u800C\u7B80\u5316\u8C03\u8BD5\u548C\u65E5\u5FD7\
  \u8BB0\u5F55\u8FC7\u7A0B\u3002"
lastmod: '2024-03-13T22:44:47.929919-06:00'
model: gpt-4-0125-preview
summary: "\u5C06\u9519\u8BEF\u4FE1\u606F\u548C\u8BCA\u65AD\u8F93\u51FA\u5199\u5165\
  \u6807\u51C6\u9519\u8BEF\uFF08stderr\uFF09\u662F\u6307\u5C06\u8FD9\u4E9B\u4FE1\u606F\
  \u5B9A\u5411\u5230\u4E00\u4E2A\u4E0E\u6807\u51C6\u8F93\u51FA\uFF08stdout\uFF09\u4E0D\
  \u540C\u7684\u72EC\u7ACB\u901A\u9053\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\
  \u4E3A\u4E86\u5C06\u5E38\u89C4\u7A0B\u5E8F\u7ED3\u679C\u548C\u9519\u8BEF\u4FE1\u606F\
  \u533A\u5206\u5F00\u6765\uFF0C\u4ECE\u800C\u7B80\u5316\u8C03\u8BD5\u548C\u65E5\u5FD7\
  \u8BB0\u5F55\u8FC7\u7A0B\u3002."
title: "\u5199\u5165\u6807\u51C6\u9519\u8BEF"
weight: 25
---

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
