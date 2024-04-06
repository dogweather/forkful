---
date: 2024-01-26 03:46:02.379343-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u4E0E\u5176\u4ED6\u4E00\u4E9B\u8BED\u8A00\
  \u4E0D\u540C\uFF0CLua\u9ED8\u8BA4\u60C5\u51B5\u4E0B\u4E0D\u5305\u62EC\u56DB\u820D\
  \u4E94\u5165\u51FD\u6570\u3002\u5386\u53F2\u4E0A\uFF0C\u4F60\u9700\u8981\u7F16\u5199\
  \u81EA\u5DF1\u7684\u6216\u4F7F\u7528\u7B2C\u4E09\u65B9\u5E93\u3002\u5E38\u89C1\u7684\
  \u89E3\u51B3\u65B9\u6848\u4F9D\u8D56\u4E8E\u4F7F\u7528 `math.floor()` \u7528\u4E8E\
  \u56DB\u820D\u4E94\u5165\u5411\u4E0B\u4EE5\u53CA `math.ceil()` \u7528\u4E8E\u56DB\
  \u820D\u4E94\u5165\u5411\u4E0A\uFF0C\u5E76\u4E14\u6839\u636E\u6570\u5B57\u7684\u7B26\
  \u53F7\uFF0C\u5728\u6267\u884C\u6B64\u64CD\u4F5C\u4E4B\u524D\u52A0\u4E0A\u6216\u51CF\
  \u53BB0.5\u3002\u2026"
lastmod: '2024-04-05T21:53:48.210729-06:00'
model: gpt-4-0125-preview
summary: "\u81EA\u5B9A\u4E49\u51FD\u6570\u7684\u66FF\u4EE3\u65B9\u6CD5\u5305\u62EC\
  \u5982 \"lua-users wiki\" \u6216 \"Penlight\" \u7B49\u5E93\u3002\u6BCF\u4E2A\u90FD\
  \u6709\u5176\u597D\u5904\u548C\u6743\u8861\uFF0C\u6BD4\u5982\u989D\u5916\u7684\u529F\
  \u80FD\u6216\u66F4\u591A\u7684\u5F00\u9500."
title: "\u6570\u5B57\u53D6\u6574"
weight: 13
---

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
