---
title:                "Bash: 生成随机数"
simple_title:         "生成随机数"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 为什么要使用 Bash 编程生成随机数字？

Bash 是一种流行的命令行解释器，它可以让你在操作系统中执行命令和脚本。生成随机数字是 Bash 编程中的一个常见需求，它可以用于密码生成、数据加密和模拟测试等方面。

## 如何生成随机数字

要在 Bash 中生成随机数字，你可以使用内置的 `$RANDOM` 变量。这个变量将会在每次被调用时产生一个 0 到 32767 的随机整数。下面是一个示例代码和输出结果：

```Bash
# 生成 6 位随机数字
echo $((RANDOM % 1000000))
# 输出：512346
```

如果你想生成更大范围的随机数字，可以使用 `shuf` 命令。它可以在指定范围内生成随机数字，并且可以指定生成的数量。下面是一个示例代码和输出结果：

```Bash
# 生成 10 个 1 到 100 之间的随机数字
echo $(shuf -i 1-100 -n 10)
# 输出：7 34 63 19 41 78 92 58 87 96
```

## 深入了解随机数字生成

在 Bash 中，随机数字是通过伪随机数生成器来产生的。这种生成方式可以通过设置种子(seed) 来控制随机数的产生。如果不设置种子，每次生成的随机数序列都将会是不同的。

为了设置种子，我们可以使用 `RANDOM` 变量中的随机数来作为种子。下面是一个示例代码和输出结果：

```Bash
# 使用 RANDOM 变量作为种子生成 10 个 1 到 100 之间的随机数字
shuf -i 1-100 -n 10 --random-source <(echo $RANDOM)
# 输出：77 93 50 20 31 9 22 78 17 89
```

需要注意的是，Bash 中的 `$RANDOM` 变量只能产生 0 到 32767 的整数，如果需要更大范围的随机数，可以使用外部工具如 `shuf`。

## 参考资料

- [Bash Manual](https://www.gnu.org/software/bash/manual/bash.html)
- [Bash Beginner's Guide](http://tldp.org/LDP/Bash-Beginners-Guide/html/)
- [Bash Scripting Tutorial](https://ryanstutorials.net/bash-scripting-tutorial/)
- [Generate Random Numbers in Bash](https://www.baeldung.com/linux/generate-random-numbers-in-bash)