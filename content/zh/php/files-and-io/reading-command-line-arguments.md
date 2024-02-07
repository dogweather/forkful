---
title:                "读取命令行参数"
date:                  2024-01-20T17:56:27.052362-07:00
model:                 gpt-4-1106-preview
simple_title:         "读取命令行参数"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? 什么 & 为什么？
在 PHP 中读取命令行参数使我们能在脚本执行时接受用户输入。程序员这么做是为了提高灵活性和交互性，让程序可以更具有适应性。

## How to: 如何做？
```PHP
<?php
// 检查是否有参数传入
if ($argc > 1) {
    echo "Hello, " . $argv[1] . "!\n";
} else {
    echo "Hello, world!\n";
}
?>
```
运行脚本 `php script.php Neo` 输出将会是 `Hello, Neo!`。

## Deep Dive 深入探索
从 PHP 4.3.0 版本开始，`$argc` 和 `$argv` 变量被引入，使得读取命令行参数变得简单。还有其他方式，像是使用 `getopt()` 函数来获取选项和参数。实现细节上，`$argc` 表示参数数量，`$argv` 是个数组包含了所有参数。在命令行运行 PHP 脚本时，第一个参数总是脚本名称。

## See Also 参见
- PHP 官方文档命令行使用: [https://www.php.net/manual/en/features.commandline.usage.php](https://www.php.net/manual/en/features.commandline.usage.php)
- `getopt()` 函数: [https://www.php.net/manual/en/function.getopt.php](https://www.php.net/manual/en/function.getopt.php)
