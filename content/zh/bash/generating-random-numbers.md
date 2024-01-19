---
title:                "生成随机数"
html_title:           "Go: 生成随机数"
simple_title:         "生成随机数"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 什么以及为什么？
生成随机数是产生不可预测值的方法。编程中常常使用它们来测试软件的效率或执行随机事件。

## 如何操作：
在Bash中，我们可以用 `$RANDOM` 来产生一个随机数，范围在0到32767之间。这是一个基本的例子：

```Bash
echo $RANDOM
```

输出:

```Bash
12529
```

如果你想在特定范围内生成随机数，比如在1到100之间，你可以像下面这样做:

```Bash
echo $(( RANDOM % 100 + 1))
```

输出:

```Bash
68
```

## 深入探讨：
`$RANDOM` 是Bash built-in的变量之一，它在1970年代由Stephen Bourne首次引入。尽管 `RANDOM` 非常便捷，在某些复杂的应用程序中，包括加密和大数据分析， 因为它的随机性并不完美，可能不是最佳选择。一些更强大的替代品，如同 `/dev/urandom` 或者 `openssl` 可用于这些场景。

在一个实现的层面，`$RANDOM` 生成随机数是基于线性同余生成器（linear congruential generator）。这是一个在老式电脑系统中很普遍的方法，但是在现代系统中可能无法提供足够的随机性。

## 另请参阅：

1. [SEI Insight on Randomness](https://resources.sei.cmu.edu/asset_files/Presentation/2010_017_001_15287.pdf)
2. [Cryptographically Secure Pseudo-random Number Generator](https://en.wikipedia.org/wiki/Cryptographically_secure_pseudorandom_number_generator)
3. [OpenSSL Rand](https://www.openssl.org/docs/manmaster/man3/RAND_bytes.html)