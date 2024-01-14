---
title:                "PHP: 读取命令行参数"
simple_title:         "读取命令行参数"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# 为什么要阅读命令行参数？

在PHP编程中，阅读命令行参数是一项非常重要的技能。通过使用命令行参数，我们可以向程序传递额外的信息，从而使得程序更加灵活和可配置。这样一来，我们就可以根据不同的需求来运行同一段代码，而无需修改代码本身。

## 如何阅读命令行参数？

阅读命令行参数并不复杂。首先，我们需要在程序中使用PHP内置的`$argv`变量来获取所有的命令行参数。然后，我们可以使用`$argv`数组来访问每一个参数。下面是一个简单的例子：

```PHP
<?php
// 获取命令行参数
$args = $argv;

// 打印出所有参数
foreach ($args as $arg) {
    echo "$arg\n";
}
```

假设我们在命令行中输入了如下命令：

```
php myprogram.php hello world
```

那么上面的代码将会输出：

```
myprogram.php
hello
world
```

我们可以看到，`$argv`数组中的第一个元素是程序的名称，而后面的参数则依次排列。如果我们想要获取特定位置的参数，可以使用`$argv`数组中的对应索引。例如，如果我们想要获取第二个参数，可以使用`$argv[1]`。

## 深入了解命令行参数

在上面的例子中，我们仅仅是简单地打印出了命令行参数。但是，命令行参数的功能远不止于此。我们还可以给参数添加前缀，如`--option`或`-o`。这样一来，我们就可以通过检查参数是否存在来决定程序的行为。

此外，我们还可以为命令行参数设置默认值。当用户没有输入参数时，程序将会使用默认值来运行。要想使用这个功能，我们需要使用`$argc`变量来获取参数的数量。如果`$argc`为1，意味着用户没有输入任何参数，我们可以使用`$args[0]`来判断用户是否添加了命令行选项。

最后，我们还可以使用第三方库来简化使用命令行参数的过程。例如，Symfony的Console组件提供了非常强大的功能，可帮助我们解析和使用命令行参数。

# 参考链接

- [PHP $argc变量的用法及算法官网示例](https://www.runoob.com/php/php-argc-argc.html)
- [使用Symphony Console组件处理命令行参数](https://www.toptal.com/php/building-chat-cli-symfony-console)
- [PHP命令行参数简介](https://www.php.net/manual/zh/features.commandline.php)

# 参见

- [PHP内置变量](https://www.php.net/manual/zh/reserved.variables.php)
- [Symphony Console组件文档](https://symfony.com/doc/current/components/console.html)