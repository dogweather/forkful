---
date: 2024-01-20 17:56:27.052362-07:00
description: "\u5728 PHP \u4E2D\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570\u4F7F\u6211\
  \u4EEC\u80FD\u5728\u811A\u672C\u6267\u884C\u65F6\u63A5\u53D7\u7528\u6237\u8F93\u5165\
  \u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u63D0\u9AD8\u7075\u6D3B\
  \u6027\u548C\u4EA4\u4E92\u6027\uFF0C\u8BA9\u7A0B\u5E8F\u53EF\u4EE5\u66F4\u5177\u6709\
  \u9002\u5E94\u6027\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.881209-06:00'
model: gpt-4-1106-preview
summary: "\u5728 PHP \u4E2D\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570\u4F7F\u6211\u4EEC\
  \u80FD\u5728\u811A\u672C\u6267\u884C\u65F6\u63A5\u53D7\u7528\u6237\u8F93\u5165\u3002\
  \u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u63D0\u9AD8\u7075\u6D3B\u6027\
  \u548C\u4EA4\u4E92\u6027\uFF0C\u8BA9\u7A0B\u5E8F\u53EF\u4EE5\u66F4\u5177\u6709\u9002\
  \u5E94\u6027\u3002"
title: "\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570"
weight: 23
---

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
