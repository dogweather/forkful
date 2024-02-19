---
aliases:
- /zh/lua/organizing-code-into-functions/
date: 2024-01-26 01:11:03.206810-07:00
description: "\u5C06\u4EE3\u7801\u7EC4\u7EC7\u6210\u51FD\u6570\uFF0C\u5C31\u662F\u5C06\
  \u4F60\u7684\u811A\u672C\u5206\u89E3\u4E3A\u53EF\u7BA1\u7406\u7684\u5C0F\u5757\u2014\
  \u2014\u60F3\u8C61\u6210\u529F\u80FD\u6027\u7684\u4E50\u9AD8\u79EF\u6728\u5757\u3002\
  \u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u6E05\u6670\u6027\u3001\u53EF\u91CD\u7528\u6027\
  \u548C\u5FC3\u667A\u5065\u5EB7\u3002\u5B83\u4F7F\u5F97\u6211\u4EEC\u7684\u4EE3\u7801\
  \u6574\u6D01\u3001\u53EF\u8BFB\u5E76\u4E14\u4FBF\u4E8E\u7EF4\u62A4\u3002"
lastmod: 2024-02-18 23:08:59.258477
model: gpt-4-1106-preview
summary: "\u5C06\u4EE3\u7801\u7EC4\u7EC7\u6210\u51FD\u6570\uFF0C\u5C31\u662F\u5C06\
  \u4F60\u7684\u811A\u672C\u5206\u89E3\u4E3A\u53EF\u7BA1\u7406\u7684\u5C0F\u5757\u2014\
  \u2014\u60F3\u8C61\u6210\u529F\u80FD\u6027\u7684\u4E50\u9AD8\u79EF\u6728\u5757\u3002\
  \u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u6E05\u6670\u6027\u3001\u53EF\u91CD\u7528\u6027\
  \u548C\u5FC3\u667A\u5065\u5EB7\u3002\u5B83\u4F7F\u5F97\u6211\u4EEC\u7684\u4EE3\u7801\
  \u6574\u6D01\u3001\u53EF\u8BFB\u5E76\u4E14\u4FBF\u4E8E\u7EF4\u62A4\u3002"
title: "\u5C06\u4EE3\u7801\u7EC4\u7EC7\u6210\u51FD\u6570"
---

{{< edit_this_page >}}

## 什么 & 为什么？
将代码组织成函数，就是将你的脚本分解为可管理的小块——想象成功能性的乐高积木块。这么做是为了清晰性、可重用性和心智健康。它使得我们的代码整洁、可读并且便于维护。

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
