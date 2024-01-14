---
title:    "Fish Shell: 生成随机数"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

为什么：随机数在编程中是非常有用的，它可以用来模拟实际情况，进行测试，或者生成随机数据。

如何使用Fish Shell生成随机数：

```Fish Shell

# 生成一个0-10的随机整数
set random_number (math /dev/urandom 0 10)
echo $random_number

# 生成一个0-1的随机小数
set random_decimal (math /dev/urandom 0 1)
echo $random_decimal

# 生成一个含有10个元素的随机数组
set -l random_array (for i in (seq 1 10); math /dev/urandom 0 100; end)
echo $random_array

```

深入了解随机数生成：

生成随机数的关键是使用操作系统提供的随机设备/dev/urandom。Fish Shell中的math函数可以从该设备中获取随机数，参数如/dev/urandom表示随机设备，后面的0和10表示随机数的范围。随机数范围的设定可以根据需求自行调整。

另外，可以使用for循环来生成随机数组，通过设置数组的长度和math函数的参数，我们可以方便地获取任意长度的随机数组。

## 另请参阅

- Fish Shell官方文档：https://fishshell.com/docs/current/
- 随机数生成器：https://www.random.org/