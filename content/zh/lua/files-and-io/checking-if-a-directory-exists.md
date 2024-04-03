---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:45.449597-07:00
description: "\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728\u662F\u7F16\u5199\u4E0E\
  \u6587\u4EF6\u7CFB\u7EDF\u4EA4\u4E92\u7684\u811A\u672C\u65F6\u7684\u57FA\u7840\u64CD\
  \u4F5C\uFF0C\u786E\u4FDD\u4F60\u7684\u7A0B\u5E8F\u5728\u6709\u6548\u8DEF\u5F84\u4E0A\
  \u64CD\u4F5C\uFF0C\u9632\u6B62\u4E0E\u4E0D\u5B58\u5728\u7684\u76EE\u5F55\u76F8\u5173\
  \u7684\u9519\u8BEF\u3002\u8FD9\u4E2A\u4EFB\u52A1\u5BF9\u4E8E\u5728\u76EE\u5F55\u4E2D\
  \u521B\u5EFA\u65B0\u6587\u4EF6\u3001\u4ECE\u4E2D\u8BFB\u53D6\u6216\u5B89\u5168\u5730\
  \u6267\u884C\u7279\u5B9A\u76EE\u5F55\u64CD\u4F5C\u81F3\u5173\u91CD\u8981\u3002"
lastmod: '2024-03-13T22:44:47.927733-06:00'
model: gpt-4-0125-preview
summary: "\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728\u662F\u7F16\u5199\u4E0E\
  \u6587\u4EF6\u7CFB\u7EDF\u4EA4\u4E92\u7684\u811A\u672C\u65F6\u7684\u57FA\u7840\u64CD\
  \u4F5C\uFF0C\u786E\u4FDD\u4F60\u7684\u7A0B\u5E8F\u5728\u6709\u6548\u8DEF\u5F84\u4E0A\
  \u64CD\u4F5C\uFF0C\u9632\u6B62\u4E0E\u4E0D\u5B58\u5728\u7684\u76EE\u5F55\u76F8\u5173\
  \u7684\u9519\u8BEF\u3002\u8FD9\u4E2A\u4EFB\u52A1\u5BF9\u4E8E\u5728\u76EE\u5F55\u4E2D\
  \u521B\u5EFA\u65B0\u6587\u4EF6\u3001\u4ECE\u4E2D\u8BFB\u53D6\u6216\u5B89\u5168\u5730\
  \u6267\u884C\u7279\u5B9A\u76EE\u5F55\u64CD\u4F5C\u81F3\u5173\u91CD\u8981\u3002."
title: "\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728"
weight: 20
---

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
