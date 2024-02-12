---
title:                "阅读文本文件"
aliases:
- zh/lua/reading-a-text-file.md
date:                  2024-01-20T17:54:48.078615-07:00
model:                 gpt-4-1106-preview
simple_title:         "阅读文本文件"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/lua/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (是什么以及为什么?)

读取文本文件就是把文件内容载入内存。程序员需要这么做来处理数据，设置配置，或处理用户输入。

## How to: (怎么做：)

```Lua
-- 打开文件
local file = io.open("example.txt", "r")
-- 检查文件是否存在
if not file then
    print("文件不存在！")
    return
end

-- 读取内容
local content = file:read("*a") -- 读取全部内容

-- 显示文件内容
print(content)

-- 关闭文件
file:close()
```

输出:
```
Hello, World!
这是一个文本文件的例子。
```

## Deep Dive (深入探索)

读取文件是 Lua 中的基础操作。1993年诞生的 Lua 如今已广泛应用于游戏开发和嵌入式场景。`io.open` 函数就是从那时候起就有的。除 `io.open` 外，有 `io.lines` 读取并迭代文件行。

选择用 "r" 打开文件，表示只读模式。也有 "w" 写入，"a" 追加。读文件时用 `file:read("*a")` 读取全部内容，`file:read("*l")` 只读一行。

Lua 文件API简单，但强大。它使用了底层的C标准I/O库，所以效率很高。

## See Also (另请参阅)

- [Lua 5.4 参考手册](https://www.lua.org/manual/5.4/)
- [Programming in Lua（第四版）](https://www.lua.org/pil/contents.html)
