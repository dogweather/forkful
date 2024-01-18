---
title:                "检查目录是否存在"
html_title:           "Lua: 检查目录是否存在"
simple_title:         "检查目录是否存在"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/lua/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？

检查目录是否存在是程序员经常进行的一项操作。它可以帮助我们确认指定的目录是否存在，从而避免发生错误或异常。例如，如果我们要在程序中读取某个目录中的文件，那么在进行读取之前，首先要检查该目录是否存在，以防止程序崩溃或出现其他意外情况。

## 如何：

```Lua
-- 使用lfs模块的attributes函数来检查目录是否存在
local exists = require "lfs".attributes("/path/to/directory")

-- 如果目录存在，则exists的值为table类型，否则为nil
if exists then
  print("目录存在！")
else
  print("目录不存在！")
end
```

## 深入探讨：

检查目录是否存在可以说是一项基本的文件系统操作。在早期的编程语言中，并没有这样的操作，需要程序员自己设计实现。而在Lua中，我们可以使用lfs模块来简单地实现此功能。另外，除了lfs模块，也可以通过调用系统命令来检查目录是否存在，但这样会比较麻烦和不直观。

## 参考链接：

- [Lua官方文档 - lfs模块](https://www.lua.org/manual/5.4/manual.html#6.12)
- [lfs模块的Github仓库](https://github.com/keplerproject/luafilesystem)