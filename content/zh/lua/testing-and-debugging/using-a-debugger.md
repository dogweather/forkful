---
aliases:
- /zh/lua/using-a-debugger/
date: 2024-01-26 03:50:46.447137-07:00
description: "\u8C03\u8BD5\u5668\u662F\u4E00\u79CD\u5DE5\u5177\uFF0C\u5B83\u5141\u8BB8\
  \u60A8\u68C0\u67E5\u548C\u63A7\u5236\u7A0B\u5E8F\u7684\u6267\u884C\uFF0C\u4F7F\u5F97\
  \ pinpoint \u51FA\u95EE\u9898\u6240\u5728\u53D8\u5F97\u5BB9\u6613\u3002\u7A0B\u5E8F\
  \u5458\u4F7F\u7528\u8C03\u8BD5\u5668\u6765\u5B9A\u4F4D bug\u3001\u7406\u89E3\u4EE3\
  \u7801\u6D41\u7A0B\uFF0C\u5E76\u786E\u4FDD\u4ED6\u4EEC\u7684\u4EE3\u7801\u5E72\u51C0\
  \u65E0\u7591\u3002"
lastmod: 2024-02-18 23:08:59.257654
model: gpt-4-0125-preview
summary: "\u8C03\u8BD5\u5668\u662F\u4E00\u79CD\u5DE5\u5177\uFF0C\u5B83\u5141\u8BB8\
  \u60A8\u68C0\u67E5\u548C\u63A7\u5236\u7A0B\u5E8F\u7684\u6267\u884C\uFF0C\u4F7F\u5F97\
  \ pinpoint \u51FA\u95EE\u9898\u6240\u5728\u53D8\u5F97\u5BB9\u6613\u3002\u7A0B\u5E8F\
  \u5458\u4F7F\u7528\u8C03\u8BD5\u5668\u6765\u5B9A\u4F4D bug\u3001\u7406\u89E3\u4EE3\
  \u7801\u6D41\u7A0B\uFF0C\u5E76\u786E\u4FDD\u4ED6\u4EEC\u7684\u4EE3\u7801\u5E72\u51C0\
  \u65E0\u7591\u3002"
title: "\u4F7F\u7528\u8C03\u8BD5\u5668"
---

{{< edit_this_page >}}

## 什么 & 为什么？
调试器是一种工具，它允许您检查和控制程序的执行，使得 pinpoint 出问题所在变得容易。程序员使用调试器来定位 bug、理解代码流程，并确保他们的代码干净无疑。

## 如何操作：
Lua 没有内置的调试器，但您可以使用外部调试器，如 ZeroBrane Studio。以下是您如何使用它的示例：

```Lua
-- 这是一个简单的 Lua 脚本，其中故意包含了一个错误
local function add(a, b)
    local result = a+ b -- 哎呀，我们假装忘记定义 'b' 了
    return result
end

print(add(10))
```

当您在调试器中运行这段代码，它会在出错的地方暂停执行。您会看到类似这样的信息：

```
lua: example.lua:3: 尝试对一个 nil 值执行算术操作（局部变量 'b'）
堆栈回溯：
	example.lua:3: 在函数 'add' 内
	example.lua:7: 在主块内
	[C]: 在 ?
```

您可以设置断点，逐步检查代码，并查看变量值，以追踪 bug 而不会丢失理智。

## 深入探索
不幸的是，Lua 的简单性并没有扩展到调试。不过没关系，Lua 社区支持您。像 ZeroBrane Studio、LuaDec 以及其他工具提供了调试功能。在历史上，调试器在第一批程序出现问题不久后就存在了，给开发者提供了一个方法来修复他们的代码，而不必盲目摸索。

对于 Lua，你经常依赖外部调试器或将它们构建到你的开发环境中。例如，ZeroBrane Studio 是一个完全集成了 Lua 调试器的 IDE。它允许你逐步执行代码、设置断点和观察变量。在实现方面，调试器通常使用钩子来插入断点和其他调试设施。

还有其他办法吗？当然。老好的 `print` 语句，亲切地称为“printf 调试”，有时候可以在没有高级工具的情况下完成任务。

## 另请参阅
要继续您的调试之旅，请查看：

- ZeroBrane Studio: https://studio.zerobrane.com/
- Lua 用户 wiki 关于调试 Lua 代码: http://lua-users.org/wiki/DebuggingLuaCode
- Lua 手册中的 `debug` 库参考资料: https://www.lua.org/manual/5.4/manual.html#6.10
