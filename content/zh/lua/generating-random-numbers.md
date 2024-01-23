---
title:                "生成随机数"
date:                  2024-01-20T17:49:27.246831-07:00
model:                 gpt-4-1106-preview
simple_title:         "生成随机数"
programming_language: "Lua"
category:             "Lua"
tag:                  "Numbers"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/lua/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (是什么 & 为什么？)
生成随机数是用程序创建不可预测数字的过程。程序员需要它们来模拟随机性，比如游戏中掷骰子，或者在统计模拟中。

## How to: (如何操作：)
```Lua
math.randomseed(os.time()) -- 初始化随机数生成器
print(math.random())       -- 输出: 一个 [0,1) 区间的随机浮点数
print(math.random(5))      -- 输出: 一个 1 到 5 区间的随机整数
print(math.random(20, 50)) -- 输出: 一个 20 到 50 区间的随机整数
```

样本输出可能是：
```
0.0012512588885159
4
35
```

## Deep Dive (深入研究)
Lua中生成随机数依赖的是`math.random`函数。首先使用`math.randomseed`来设定种子，通常是当前时间，确保每次结果不同。它背后用的是伪随机数生成器(PRG)，基于数学算法，不是真正的随机，但足够模拟随机性。

替代方案包括开源库，如`mwc-random`, `xorshift`等，它们使用不同的算法提供随机数。一些高安全需求场景可能会使用硬件随机数生成器。

历史上，随机数生成已长久存在，从简单的机械设备到复杂的算法都有。在计算机中，随机数的使用是多方面的，如游戏、模拟、安全算法等。

## See Also (另请参阅)
- Lua 5.4 参考手册：https://www.lua.org/manual/5.4/
- Lua 用户维基：http://lua-users.org/wiki/MathLibraryTutorial
- LuaRocks上的随机数库：https://luarocks.org/search?q=random
