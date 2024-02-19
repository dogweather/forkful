---
aliases:
- /zh/lua/working-with-complex-numbers/
date: 2024-01-26 04:43:14.987854-07:00
description: "\u590D\u6570\u901A\u8FC7\u5305\u542B\u4E00\u4E2A\u5782\u76F4\u7684\u865A\
  \u8F74\uFF0C\u5C06\u4E00\u7EF4\u6570\u7EBF\u7684\u6982\u5FF5\u6269\u5C55\u5230\u4E86\
  \u4E8C\u7EF4\u5E73\u9762\u3002\u7A0B\u5E8F\u5458\u5728\u8BF8\u5982\u4FE1\u53F7\u5904\
  \u7406\u3001\u6D41\u4F53\u52A8\u529B\u5B66\u548C\u7535\u6C14\u5DE5\u7A0B\u7B49\u9886\
  \u57DF\u5DE5\u4F5C\u65F6\uFF0C\u4F1A\u4F7F\u7528\u5230\u5B83\u4EEC\uFF0C\u56E0\u4E3A\
  \u5B83\u4EEC\u5BF9\u4E8E\u8868\u5F81\u632F\u8361\u548C\u5176\u4ED6\u73B0\u8C61\u662F\
  \u5FC5\u4E0D\u53EF\u5C11\u7684\u3002"
lastmod: 2024-02-18 23:08:59.248292
model: gpt-4-0125-preview
summary: "\u590D\u6570\u901A\u8FC7\u5305\u542B\u4E00\u4E2A\u5782\u76F4\u7684\u865A\
  \u8F74\uFF0C\u5C06\u4E00\u7EF4\u6570\u7EBF\u7684\u6982\u5FF5\u6269\u5C55\u5230\u4E86\
  \u4E8C\u7EF4\u5E73\u9762\u3002\u7A0B\u5E8F\u5458\u5728\u8BF8\u5982\u4FE1\u53F7\u5904\
  \u7406\u3001\u6D41\u4F53\u52A8\u529B\u5B66\u548C\u7535\u6C14\u5DE5\u7A0B\u7B49\u9886\
  \u57DF\u5DE5\u4F5C\u65F6\uFF0C\u4F1A\u4F7F\u7528\u5230\u5B83\u4EEC\uFF0C\u56E0\u4E3A\
  \u5B83\u4EEC\u5BF9\u4E8E\u8868\u5F81\u632F\u8361\u548C\u5176\u4ED6\u73B0\u8C61\u662F\
  \u5FC5\u4E0D\u53EF\u5C11\u7684\u3002"
title: "\u5904\u7406\u590D\u6570"
---

{{< edit_this_page >}}

## 什么和为什么？
复数通过包含一个垂直的虚轴，将一维数线的概念扩展到了二维平面。程序员在诸如信号处理、流体动力学和电气工程等领域工作时，会使用到它们，因为它们对于表征振荡和其他现象是必不可少的。

## 如何操作：
在Lua中，你可以用表格来表示复数。基本操作包括加、减、乘、除这些表格。以下是如何操作：

```lua
-- 定义两个复数为表格
local complex_a = { real = 3, imag = 5 }
local complex_b = { real = 2, imag = -4 }

-- 用于添加两个复数的函数
local function add_complex(a, b)
  return { real = a.real + b.real, imag = a.imag + b.imag }
end

-- 示例输出
print(add_complex(complex_a, complex_b))  -- { real = 5, imag = 1 }
```

## 深入探讨
复数自16世纪以来已经存在，帮助解决了仅用实数无法解的方程。Lua本身没有内建的复数类型。然而，这不是大问题——正如上面所展示的，你可以使用表格和函数巧妙地进行自己的复数操作。或者，如果你有更深入的需要，可以捕获像LuaComplex这样的库。这是一个不错的选择，因为它是专门为Lua构建的，并且减轻了你的手动工作。像这样的库通常还会在后台优化操作，因此它们比自己动手编写要快。

## 另请参阅
欲了解更多详细示例和高级操作，请查看：

- LuaComplex库：https://github.com/davidm/lua-complex
- 《Lua编程》书籍，关于自定义数据类型创建：https://www.lua.org/pil/11.1.html
- 维基百科上不同领域中复数的应用：https://en.wikipedia.org/wiki/Complex_number#Applications
