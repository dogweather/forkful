---
title:    "Bash: 生成随机数"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 为什么要使用 Bash 编程

Bash 是一个功能强大的编程语言，它可以作为命令行工具来生成随机数。这对于编程和测试中的多种用途非常有用，比如生成测试数据、创建密码、模拟随机事件等。让我们来看看如何使用 Bash 来生成随机数吧。

## 如何生成随机数

首先，我们需要使用 `dev/urandom` 设备来生成随机数。我们可以通过在终端中输入以下命令来生成一个随机数：

```Bash
cat /dev/urandom | tr -dc '0-9' | head -c 10
```

上面的命令会将 `dev/urandom` 设备中的无限流的随机数转换成数字，并且只会保留 10 位数字。你也可以改变最后的 `head -c` 参数来生成更长或者更短的随机数。

如果你想要生成一个 6 位数字的随机密码，可以使用以下命令：

```Bash
cat /dev/urandom | tr -dc 'a-zA-Z0-9' | head -c 6
```

这个命令会从 `dev/urandom` 设备中生成包含字母和数字的随机数，并且只会保留前 6 位字符。你可以根据自己的需要修改 `head -c` 中的数字来改变密码的长度。

## 深入了解随机数生成

关于随机数生成的过程，还有很多值得我们深入了解的内容。比如，随机数实际上是一种伪随机数，它是由一个称为随机数发生器的程序来生成的。这个程序会根据一个称为种子的初始值，来产生一个看似随机的数字序列。种子的值决定了随机数的序列，因此，如果两次使用相同的种子值，就会生成相同的随机数序列。

这就是为什么在使用 `dev/urandom` 生成随机数时，我们需要使用系统的熵池作为种子。熵池会收集来自系统活动的随机数据，比如键盘输入、鼠标移动、网络活动等。这样，每一次生成的随机数序列都会因为种子值的不同而产生差异。

## 参考链接

- [Bash 编程入门](https://linuxhandbook.com/bash-programming/)
- [随机数生成器的工作原理](https://www.geeksforgeeks.org/pseudo-random-number-generator-prng/)
- [如何生成随机密码](https://www.howtogeek.com/howto/30184/10-ways-to-generate-a-random-password-from-the-command-line/)
- [Linux 熵池介绍](https://bencane.com/2012/12/03/understanding-linux-entropy-pool/)
- [命令行生成随机数的其他方法](https://www.tecmint.com/generate-random-passwords-in-linux/)