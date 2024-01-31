---
title:                "编写文本文件"
date:                  2024-01-19
html_title:           "Arduino: 编写文本文件"
simple_title:         "编写文本文件"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/lua/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?
写文本文件让你保存数据到硬盘。程序员这么做可以持久保存信息，比如配置、日志。

## 如何:
```Lua
-- 打开文件
local file = io.open("example.txt", "w")
if not file then return end

-- 写入内容
file:write("你好，世界！\n")

-- 关闭文件
file:close()
```
运行后查看`example.txt`，内容是`你好，世界！`。

## 深入探讨:
Lua写文件，源自C语言`stdio`库。除了`io.open`，`io.popen`等命令也可写文件，提供更多控制。Lua5.4用`io.close(file)`更安全地关闭文件。

## 参见:
- [Lua 5.4 参考手册](https://www.lua.org/manual/5.4/)
- [Programming in Lua (第四版)](https://www.lua.org/pil/contents.html)
