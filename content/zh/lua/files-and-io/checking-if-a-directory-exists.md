---
title:                "检查目录是否存在"
date:                  2024-02-03T19:07:45.449597-07:00
model:                 gpt-4-0125-preview
simple_title:         "检查目录是否存在"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/lua/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么?

检查目录是否存在是编写与文件系统交互的脚本时的基础操作，确保你的程序在有效路径上操作，防止与不存在的目录相关的错误。这个任务对于在目录中创建新文件、从中读取或安全地执行特定目录操作至关重要。

## 如何操作:

在Lua中，没有内置函数可以直接检查目录是否存在，因此你通常依赖于Lua文件系统（lfs）库，这是一个流行的第三方文件操作库。

首先，确保你安装了Lua文件系统。如果没有，你通常可以使用LuaRocks安装:

```sh
luarocks install luafilesystem
```

然后，你可以使用以下示例来检查目录是否存在：

```lua
local lfs = require "lfs"

function directoryExists(directory)
    local attr = lfs.attributes(directory)
    return attr and attr.mode == "directory"
end

-- 检查一个特定目录是否存在
if directoryExists("/path/to/your/directory") then
    print("目录存在。")
else
    print("目录不存在。")
end
```

这将输出：

```
目录存在。
```

或者，如果目录不存在：

```
目录不存在。
```

这种方法使用了`lfs.attributes`函数来获取路径的属性。如果路径存在且其`mode`属性为`directory`，则确认目录的存在。
