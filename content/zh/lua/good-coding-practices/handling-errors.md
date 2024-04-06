---
date: 2024-01-26 00:56:01.161101-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Lua \u4E3B\u8981\u4F7F\u7528\u4E24\u4E2A\
  \u51FD\u6570\u8FDB\u884C\u9519\u8BEF\u5904\u7406\uFF1A`pcall` \u548C `xpcall`\u3002\
  \u4E0B\u9762\u662F\u5982\u4F55\u4F7F\u7528\u5B83\u4EEC\uFF1A."
lastmod: '2024-04-05T21:53:48.224122-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u5904\u7406\u9519\u8BEF"
weight: 16
---

## 如何操作：
Lua 主要使用两个函数进行错误处理：`pcall` 和 `xpcall`。下面是如何使用它们：

```lua
function might_fail()
    if math.random() > 0.5 then
        error("糟糕！出错了。")
    else
        print("一切正常！")
    end
end

-- 使用 pcall
local success, errorMessage = pcall(might_fail)

if success then
    print("成功！")
else
    print("捕获到错误：", errorMessage)
end

-- 使用带有错误处理器的 xpcall
function myErrorHandler(err)
    print("错误处理器说：", err)
end

local status = xpcall(might_fail, myErrorHandler)
print("调用是否成功？", status)
```

示例输出可能是：

```
捕获到错误：糟糕！出错了。
错误处理器说：糟糕！出错了。
调用是否成功？false
```
或者，如果没有发生错误：
```
一切正常！
成功！
一切正常！
调用是否成功？true
```

## 深入了解
处理错误或“异常处理”并非始终如一。早期程序经常崩溃。随着编程的发展，对稳定性的需求也在增长。Lua的方法与一些语言相比很简单。没有 `try/catch` 块，只有 `pcall` 和 `xpcall`。前者保护一个函数调用，返回一个状态和任何错误。后者增加了一个错误处理函数，对于自定义清理或记录很有用。

Lua 的另一个选择是使用 `assert`，它可以通过在条件为假时抛出错误来发挥类似作用。但对于复杂的错误处理场景，它没有 `pcall` 灵活。

在内部，`pcall` 和 `xpcall` 通过设置一个函数运行的“受保护环境”来工作。如果出现错误，环境会捕获它，并且可以立即处理它或将其传回给程序处理。

## 另请参阅
- 《Lua 编程》书籍（第三版），可在 https://www.lua.org/pil/ 阅读有关错误处理的详尽信息（第8.4节）。
- 官方 Lua 5.4 参考手册：https://www.lua.org/manual/5.4/ - 关于 Lua 错误处理功能的最新信息。
- Lua 用户社区关于错误处理的维基：http://lua-users.org/wiki/ErrorHandling - 获取社区洞见和模式。
