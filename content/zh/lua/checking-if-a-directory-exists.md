---
title:                "检查目录是否存在"
html_title:           "PowerShell: 检查目录是否存在"
simple_title:         "检查目录是否存在"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/lua/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 什么以及为什么？
检查目录是否存在是一个常见的编程任务，它是指通过编程代码来确定某个特定的文件夹是否在计算机上实际存在。它对于避免在尝试访问不存在的目录时出错，或者在需要创建新目录时避免重复创建非常有用。

## 如何做：
在Lua中，我们可以使用`lfs`（Lua文件系统）库来检查目录是否存在。以下是如何做的示例：
```Lua
local lfs = require('lfs')

function is_dir(path)
    -- lfs.attributes(path, 'mode')如果路径存在，就返回路径的模式（文件或目录），否则返回nil
    return lfs.attributes(path, 'mode') == 'directory'
end

--测试用例
print(is_dir('/path/to/directory'))  --如果目录存在，返回 true，否则返回 false
```
## 深入探讨
在早些时候的Lua版本中，没有内置函数来检查目录是否存在。我们必须依赖于系统的特定命令或者其他库。然而，在最新版本中，Lua文件系统`lfs`库提供了这个功能，并且通过`lfs.attributes(path, 'mode')`函数，它可以很容易地实现。

`lfs.attributes(path, 'mode')`函数的工作原理是首先尝试获取路径的所有属性，然后通过'mode'参数返回文件类型。如果路径不存在，则函数返回nil。

除了`lfs`库，还有其他一些库（如`posix`）也可以实现检查目录是否存在的功能。但是，由于`lfs`库的简单和便利，它通常是首选。

## 另见
有关`lfs`库和`lfs.attributes`函数的更多详细信息，请参阅：
- Lua文件系统： https://keplerproject.github.io/luafilesystem/
- Lua文件系统手册：https://keplerproject.github.io/luafilesystem/manual.html#attributes
如果你想要了解其他库或方法的更多信息，你可以参考：
- `posix`库：https://github.com/luaposix/luaposix