---
title:                "检查目录是否存在"
date:                  2024-01-20T14:57:50.852192-07:00
html_title:           "Elixir: 检查目录是否存在"
simple_title:         "检查目录是否存在"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/lua/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? 什么以及为什么？
检查目录是否存在是一种验证特定路径下文件夹是否存在的操作。程序员这么做是为了避免在操作不存在的文件夹时出现错误，保证程序的稳定性和数据的安全性。

## How to 怎么做
Lua自身不提供直接检测目录是否存在的功能，但我们可以通过调用操作系统的命令或使用第三方库来实现。

```Lua
local lfs = require('lfs') -- 引入luafilesystem库

function directoryExists(directory)
    local ok, error, code = os.rename(directory, directory)
    if not ok then
       if code == 13 then
          -- 代码13表示权限不足，即路径存在
          return true
       end
    end
    return ok, error
end

-- 使用函数检查目录
if directoryExists("/path/to/directory") then
    print("目录存在！")
else
    print("目录不存在或无法访问。")
end
```

**样本输出：**
```
目录存在！
```
或者
```
目录不存在或无法访问。
```

## Deep Dive 深入探讨
自Lua 5.1起，luafilesystem第三方库被广泛使用来处理文件和目录。尽管有其他方法，比如调用`os.execute`来运行系统命令或使用`io.popen`，但这些方法可能会因操作系统不同而变得不可靠。luafilesystem提供了一个跨平台的解决方案。

而在历史上，Lua开发者经常依赖于操作系统特定的指令来间接得到文件系统的信息，这显得不够优雅且容易出错。现在，使用`lfs`可以更简单地实现跨平台的文件系统操作，如检查目录是否存在。

`os.rename`函数通常用于重命名文件或目录。但在这里，我们用它来尝试重命名目标目录到自身，如果目录不存在或无法访问，它会返回一个错误。这是一种利用现有工具的创新做法，但请注意，它可能不会检测隐藏目录或系统保护的目录。

## See Also 查看更多
- Lua 5.4参考手册：[https://www.lua.org/manual/5.4/](https://www.lua.org/manual/5.4/)
- LuaRocks —— Lua的包管理器：[https://luarocks.org/](https://luarocks.org/)
