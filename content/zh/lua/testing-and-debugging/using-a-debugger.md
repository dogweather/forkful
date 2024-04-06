---
date: 2024-01-26 03:50:46.447137-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Lua \u6CA1\u6709\u5185\u7F6E\u7684\u8C03\
  \u8BD5\u5668\uFF0C\u4F46\u60A8\u53EF\u4EE5\u4F7F\u7528\u5916\u90E8\u8C03\u8BD5\u5668\
  \uFF0C\u5982 ZeroBrane Studio\u3002\u4EE5\u4E0B\u662F\u60A8\u5982\u4F55\u4F7F\u7528\
  \u5B83\u7684\u793A\u4F8B\uFF1A."
lastmod: '2024-04-05T21:53:48.221493-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u4F7F\u7528\u8C03\u8BD5\u5668"
weight: 35
---

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
