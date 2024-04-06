---
date: 2024-01-20 17:54:48.078615-07:00
description: "How to: (\u600E\u4E48\u505A\uFF1A) \u8BFB\u53D6\u6587\u4EF6\u662F Lua\
  \ \u4E2D\u7684\u57FA\u7840\u64CD\u4F5C\u30021993\u5E74\u8BDE\u751F\u7684 Lua \u5982\
  \u4ECA\u5DF2\u5E7F\u6CDB\u5E94\u7528\u4E8E\u6E38\u620F\u5F00\u53D1\u548C\u5D4C\u5165\
  \u5F0F\u573A\u666F\u3002`io.open` \u51FD\u6570\u5C31\u662F\u4ECE\u90A3\u65F6\u5019\
  \u8D77\u5C31\u6709\u7684\u3002\u9664 `io.open` \u5916\uFF0C\u6709 `io.lines` \u8BFB\
  \u53D6\u5E76\u8FED\u4EE3\u6587\u4EF6\u884C\u3002 \u9009\u62E9\u7528 \"r\" \u6253\
  \u5F00\u6587\u4EF6\uFF0C\u8868\u793A\u53EA\u8BFB\u6A21\u5F0F\u3002\u4E5F\u6709 \"\
  w\"\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:38:47.088584-06:00'
model: gpt-4-1106-preview
summary: "(\u600E\u4E48\u505A\uFF1A) \u8BFB\u53D6\u6587\u4EF6\u662F Lua \u4E2D\u7684\
  \u57FA\u7840\u64CD\u4F5C\u30021993\u5E74\u8BDE\u751F\u7684 Lua \u5982\u4ECA\u5DF2\
  \u5E7F\u6CDB\u5E94\u7528\u4E8E\u6E38\u620F\u5F00\u53D1\u548C\u5D4C\u5165\u5F0F\u573A\
  \u666F\u3002`io.open` \u51FD\u6570\u5C31\u662F\u4ECE\u90A3\u65F6\u5019\u8D77\u5C31\
  \u6709\u7684\u3002\u9664 `io.open` \u5916\uFF0C\u6709 `io.lines` \u8BFB\u53D6\u5E76\
  \u8FED\u4EE3\u6587\u4EF6\u884C\u3002"
title: "\u9605\u8BFB\u6587\u672C\u6587\u4EF6"
weight: 22
---

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
