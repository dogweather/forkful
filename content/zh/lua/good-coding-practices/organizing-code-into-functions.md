---
date: 2024-01-26 01:11:03.206810-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u51FD\u6570\u53EF\u4EE5\u53D8\u5F97\u66F4\
  \u52A0\u590D\u6742\uFF0C\u5904\u7406\u4E0D\u540C\u7684\u4EFB\u52A1\uFF1A."
lastmod: '2024-04-05T21:53:48.222229-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u5C06\u4EE3\u7801\u7EC4\u7EC7\u6210\u51FD\u6570"
weight: 18
---

## 如何操作：
```Lua
-- 定义一个简单的打招呼函数
function greet(name)
    return "Hello, " .. name .. "!"
end

-- 使用该函数
print(greet("Lua 程序员")) -- 样例输出：Hello, Lua 程序员！
```

函数可以变得更加复杂，处理不同的任务：
```Lua
-- 一个计算矩形面积的函数
function calculateArea(width, height)
    return width * height
end

-- 调用函数并打印结果
local area = calculateArea(5, 4)
print(area)  -- 样例输出：20
```

## 深入探索
自90年代问世以来，Lua一直鼓励模块化设计。使用函数组织代码并不是Lua独有的——自编程语言如Fortran和Lisp诞生之初，这种做法就已存在。类似内联代码和复制粘贴同样代码的替代方法不仅被人嫌弃；它们还可能是潜在的错误巢穴。

在Lua中，函数是一等公民，意味着它们可以被存储在变量中、作为参数传递，以及从其他函数中返回。它们具有多用途性。Lua的单线程特性意味着你必须保持函数精简且高效以提升性能。函数可以是局部的（有作用域的）或全局的，理解何时使用每一种可以决定你的脚本效率的成败。

## 另见
- 关于函数的官方Lua文档：https://www.lua.org/pil/6.html
- Lua中函数使用的实际例子：https://lua-users.org/wiki/SampleCode
- Lua中的清晰代码实践：https://github.com/Olivine-Labs/lua-style-guide
