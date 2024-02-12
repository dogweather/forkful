---
title:                "数字取整"
aliases: - /zh/lua/rounding-numbers.md
date:                  2024-01-26T03:46:02.379343-07:00
model:                 gpt-4-0125-preview
simple_title:         "数字取整"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/lua/rounding-numbers.md"
---

{{< edit_this_page >}}

## 什么和为什么？
四舍五入数字意味着将它们调整到最近的整数或指定的小数位。在编程中，这是一个基本操作，用于减少复杂性，提高性能，以及在超出某个点的精确度不再增加价值时。

## 如何操作：
```lua
-- Lua中的基本四舍五入操作并不内置，但你可以定义一个函数：

function round(num)
    return num >= 0 and math.floor(num + 0.5) or math.ceil(num - 0.5)
end

print(round(3.5))  -- 4
print(round(2.3))  -- 2
print(round(-1.6)) -- -2

-- 要四舍五入到特定的小数位：
function round(num, decimalPlaces)
    local mult = 10^(decimalPlaces or 0)
    return math.floor(num * mult + 0.5) / mult
end

print(round(3.14159, 2)) -- 3.14
print(round(1.98765, 3))  -- 1.988
```

## 深入了解
与其他一些语言不同，Lua默认情况下不包括四舍五入函数。历史上，你需要编写自己的或使用第三方库。常见的解决方案依赖于使用 `math.floor()` 用于四舍五入向下以及 `math.ceil()` 用于四舍五入向上，并且根据数字的符号，在执行此操作之前加上或减去0.5。

自定义函数的替代方法包括如 "lua-users wiki" 或 "Penlight" 等库。每个都有其好处和权衡，比如额外的功能或更多的开销。

在内部，这些函数通常通过利用计算机存储浮点数的方式来工作。对于你想要四舍五入的正浮点数加上0.5，将会推动它超过下一个整数值的阈值，所以当你应用 `math.floor()` 时，它会四舍五入到那个最近的整数。

## 参见
- [Lua 5.4 参考手册：数学函数](https://www.lua.org/manual/5.4/manual.html#6.7)
- [Penlight Lua库：数学](https://github.com/lunarmodules/Penlight)
