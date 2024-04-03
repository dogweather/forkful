---
date: 2024-01-20 17:56:27.052362-07:00
description: "How to: \u5982\u4F55\u505A\uFF1F ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.881209-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570"
weight: 23
---

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
