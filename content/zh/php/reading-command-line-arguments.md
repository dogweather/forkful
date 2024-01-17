---
title:                "读取命令行参数"
html_title:           "PHP: 读取命令行参数"
simple_title:         "读取命令行参数"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# 什么是读取命令行参数？为什么程序员要这么做？

读取命令行参数是指通过命令行获取用户输入的信息。这对于程序员来说非常重要，因为它允许他们根据用户输入的不同参数来执行不同的操作，从而使程序更加灵活和可控。

# 怎么做？

在PHP中，有两种主要的方式来读取命令行参数。首先是使用全局变量`$argv`，它是一个包含所有命令行参数的数组。以下是一个简单的示例：

```
<?php
// 获取第一个参数
$parameter = $argv[1];

// 输出
echo $parameter;
```

如果你在命令行中输入`php example.php hello`，那么输出将会是`hello`。

另一种方式是使用`getopt()`函数，它允许你指定期望的参数以及它们的简写形式。以下是一个示例：

```
<?php
// 定义所需的参数和选项
$options = getopt("p:ht:");

// 输出
echo $options['p'] . ", " . $options['h'] . ", " . $options['t'];
```

如果你在命令行中输入`php example.php -p hello -h -t 123`，那么输出将会是`hello, 1, 23`。

# 深入探讨

从历史的角度来看，命令行参数在早期的操作系统中非常重要，因为它是唯一的用户和操作系统交互的方式。随着图形界面的发展，它的重要性有所下降，但仍然是程序员不可或缺的工具。

除了使用全局变量`$argv`和`getopt()`函数，还有其他一些第三方库可以帮助读取和解析命令行参数，比如`Symfony Console`和`GetOptionKit`。

在实现命令行参数的过程中，需要注意处理用户错误输入的情况，以及如何处理多个参数和选项的组合。

# 参考链接

- PHP官方文档：https://www.php.net/manual/en/reserved.variables.argv.php
- PHP官方文档：https://www.php.net/manual/en/function.getopt.php
- Symfony Console库：https://symfony.com/doc/current/components/console.html
- GetOptionKit库：https://github.com/c9s/GetOptionKit