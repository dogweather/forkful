---
title:                "生成随机数"
date:                  2024-01-20T17:49:14.777066-07:00
model:                 gpt-4-1106-preview
simple_title:         "生成随机数"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (是什么以及为什么？)
生成随机数就是创建没有确定模式的数字序列。程序员用它来模拟和测试，例如游戏中的随机事件或安全的密码生成。

## How to (如何操作)
```Fish Shell
# 生成一个 1 到 100 的随机数
set random_number (random 1 100)
echo $random_number
```
输出示例:
```
42
```
```Fish Shell
# 生成 5 个随机数，范围是 1 到 100
for i in (seq 5)
    echo (random 1 100)
end
```
输出示例:
```
8
77
32
50
91
```

## Deep Dive (深入了解)
Fish Shell 的 `random` 命令是基于伪随机数生成器 (PRNG)；这意味着数字序列是由算法生成的，表面上看随机，但实际上如果你知道算法的状态，你就能预测出来。在历史上，多种PRNG方法被开发出来，比如线性同余生成器（LCG）或梅森旋转算法（MT19937），但他们有不同的性能和随机质量。在安全领域，使用加密安全的随机数生成器（CSPRNG）更加常见。

## See Also (另请参阅)
- Fish Shell 官方文档中的 `random` 命令: [https://fishshell.com/docs/current/cmds/random.html](https://fishshell.com/docs/current/cmds/random.html)
- 维基百科关于伪随机数生成器: [https://en.wikipedia.org/wiki/Pseudorandom_number_generator](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)
- CSPRNG 在维基百科上的解释: [https://en.wikipedia.org/wiki/Cryptographically_secure_pseudorandom_number_generator](https://en.wikipedia.org/wiki/Cryptographically_secure_pseudorandom_number_generator)
