---
title:                "生成随机数"
date:                  2024-01-20T17:48:49.331934-07:00
model:                 gpt-4-1106-preview
simple_title:         "生成随机数"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? 什么以及为什么？

生成随机数就是创建不可预测的数字。程序员用它们来增加加密的强度、模拟数据、执行随机测试。

## How to 怎么做

```Bash
# 生成一个在1到100之间的随机数
$ echo $(( RANDOM % 100 + 1 ))

# 示范输出
$ 57
```

```Bash
# 利用$RANDOM和date命令生成一个更复杂的随机数
$ echo $(( RANDOM % 100 + 1 ))$(date +%N | sed 's/^.\{5\}\(.\{5\}\).*$/\1/')

# 示范输出
$ 5723496
```

## Deep Dive 深入了解

随机数在计算机中的生成并非真正的随机，更常是"伪随机"。计算机使用特定算法（如线性同余生成器）来生成可预测的数字序列，但看起来足够随机。

Bash 中，我们通常用 `$RANDOM` 环境变量来生成随机数，但它有限制，比如数值范围限在0到32767之间。这样的生成方式对于简单的应用来说已经足够，但对于需要高质量随机数的应用，如加密，我们可能需要另寻他法。

`/dev/random` 和 `/dev/urandom` 是两个在Unix系统中可用的设备，它们能提供更高质量的随机数。`/dev/random` 可以提供更高的安全性，但在没有足夠熵的情况下会阻塞；而 `/dev/urandom` 生成速度更快，但安全性稍低。

其他替代方法还包括使用 OpenSSL 或者其他专门的工具来生成随机数。

## See Also 查看更多

- Bash手册: https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameters
- Wikipedia上关于`/dev/random` 和 `/dev/urandom`的条目: https://en.wikipedia.org/wiki//dev/random
- OpenSSL官方文档关于生成随机数的部分: https://www.openssl.org/docs/manmaster/man7/RAND.html
