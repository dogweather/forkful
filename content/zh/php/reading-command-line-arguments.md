---
title:                "读取命令行参数"
html_title:           "C: 读取命令行参数"
simple_title:         "读取命令行参数"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
命令行参数读取是程序获取命令行中输入信息的方式。程序员需要使用命令行参数来允许用户定义程序的运行方式。

## 如何实现：
```PHP
<?php
$arguments = $argv;
array_shift($arguments); 
foreach($arguments as $key => $value) {
    echo "参数$key 是 $value . \n";
}
?>
```

在命令行中运行它，比如 `php file.php A B C`，你将看到以下输出：

```
参数1 是 A . 
参数2 是 B . 
参数3 是 C .
```

## 深入解析:
早在命令行界面（CLI）主导计算机操作的年代，命令行参数就已经被广泛使用。尽管现在图形用户界面已经大规模普及，但命令行参数在许多场景中依然十分重要，例如在脚本、自动化任务或服务器环境中。

在PHP中，除了 `$argv` 之外还有一些获取命令行参数的方法。例如可以使用 getopt 函数来以更加灵活的方式处理命令行参数。

而 `$argv` 是一个内置数组，它的第一个元素总是包含自身脚本的名称。对于其他的命令行参数，PHP会将它们当作字符串存储在此数组中的后续元素里。

## 相关资源：
这些是一些有关PHP命令行参数的其他资源：

1. PHP官方文档中关于命令行用法的页面: [https://php.net/manual/cli.php](https://php.net/manual/cli.php)
2. PHP官方文档中关于 `getopt` 函数的页面：[https://php.net/manual/function.getopt.php](https://php.net/manual/function.getopt.php)
3. 一个关于PHP命令行参数处理的细致教程：[https://www.sitepoint.com/php-command-line-1/](https://www.sitepoint.com/php-command-line-1/)