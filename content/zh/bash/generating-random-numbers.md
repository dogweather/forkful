---
title:                "Bash: 产生随机数"
programming_language: "Bash"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

# 为什么
生成随机数是一种常用的编程技巧，有时候我们需要随机的数据来测试程序或者创建随机密码。Bash 提供了一种简单的方式来生成随机数，让我们一起来看看它是如何工作的吧。

## 如何生成随机数
Bash 有一个内置的 `$RANDOM` 变量，它可以返回一个 0 到 32767 之间的随机整数。
```
Bash script:
echo $RANDOM
```

这个脚本会打印出一个随机整数，例如：
```
13456
```

我们也可以通过使用`$RANDOM` 变量来生成一段随机数字，例如，我们想要生成一个 4 位数的随机验证码：
```
Bash script:
echo "$((RANDOM % 9000 + 1000))"
```

这个脚本会打印出一个四位数的随机验证码，例如：
```
5632
```

需要注意的是，每次运行脚本都会生成一个不同的随机数。

## 深入研究
Bash 中的 `$RANDOM` 变量实质上是一个伪随机数发生器。它使用了 Bash 进程的 ID 和当前的系统时间作为种子来生成随机数。这意味着，在同一秒内，运行相同的脚本会生成相同的随机数。因此，如果我们想要更加随机的结果，我们可以在生成随机数之前，添加一些其他的随机因素，例如随机的延迟或者从一个随机的文件中读取内容。

此外，我们还可以使用其他的 Bash 内置命令来生成随机数，例如使用 `shuf` 命令从文本文件中随机选择一行或者从 openSSL 中生成随机数。

# 查看也可以
- [Bash 官方文档关于 $RANDOM 变量的介绍](https://www.gnu.org/software/bash/manual/html_node/Bash-Variables.html)
- [Linux 命令行中随机数生成的更多技巧](https://www.tecmint.com/generate-random-numbers-strings-linux/)
- [通过在 Bash 脚本中使用随机数来提高程序的安全性](https://www.linuxjournal.com/content/generating-random-numbers-bash-scripting)