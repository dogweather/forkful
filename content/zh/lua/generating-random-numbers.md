---
title:                "生成随机数"
html_title:           "Go: 生成随机数"
simple_title:         "生成随机数"
programming_language: "Lua"
category:             "Lua"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/lua/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？

随机数生成是产生一个无法预测的数字序列的过程。程序员通常进行随机数生成来实现一些需要元素随机性的功能，如验证码生成、游戏内的随机事件等。

## 如何操作：

Lua语言提供了一个内建函数`math.random()`来生成随机数，接下里是一些基本使用方法：

```lua
-- 生成一个位于[0,1)之间的浮点数
local rand_num = math.random()
print(rand_num)
```

输出会是这样：

```lua
0.87433252131326
```

如果你需要一个位于特定范围的随机整数，可以将范围作为参数输入：

```lua
-- 生成一个[1,100]之间的整数
local rand_num = math.random(1, 100)
print(rand_num)
```

得到的结果可能是：

```lua
42
```

## 深入了解

历史上，Lua的随机数生成一直依赖于C库。然而，从Lua 5.4版本开始，使用了新的策略，包括一个新的随机数发生器（使用合理多变量序列的线性同余生成器）和一个新的随机数种子初始化策略。现在的随机数生成既可以满足绝大部分的随机需求，也提高了对安全应用的支持。

生成随机数的另一种方法是利用操作系统程序中的随机性，虽然实现细节在不同系统间有所不同。例如，一些UNIX系统提供了`/dev/urandom`设备，可以读取以获取随机字节。

## 参考资料

[Lua 5.4参考手册](http://www.lua.org/manual/5.4/)  
[Lua-users教程:随机数](http://lua-users.org/wiki/MathLibraryTutorial)  
[维基百科: 随机数生成](https://zh.wikipedia.org/wiki/隨機數_生成)