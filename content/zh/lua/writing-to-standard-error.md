---
title:                "写入标准错误"
date:                  2024-01-19
simple_title:         "写入标准错误"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/lua/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
标准错误（stderr）是特殊的输出流，用于记录错误信息而非常规数据。程序员这样做可以分离错误日志，便于调试和日志分析。

## 如何：
```Lua
-- 打印到标准错误
io.stderr:write("发生错误!\n")

-- assert 函数在遇到错误时也会写入到标准错误
assert(nil, "这是一个错误信息\n")

-- 使用 os.execute 在错误时捕获程序返回
os.execute("ls /nonexistent-directory 2>/dev/null")
```
输出：
```
发生错误!
lua: main.lua:4: 这是一个错误信息
stack traceback:
	main.lua:4: in main chunk
	[C]: in ?
```

## 深入：
1. 历史背景：Unix 系统设计时引入了标准错误概念，旨在与标准输出（stdout）分离。
2. 替代方案：可以将错误信息写入文件或使用日志库进行管理。
3. 实现细节：Lua 使用 io 库直接操作 stderr。在底层，stderr 通常对应文件描述符2。

## 参考资料：
- [Lua 5.4 参考手册](https://www.lua.org/manual/5.4/)
- [Programming in Lua](https://www.lua.org/pil/)
- [Unix 标准流](https://en.wikipedia.org/wiki/Standard_streams)
