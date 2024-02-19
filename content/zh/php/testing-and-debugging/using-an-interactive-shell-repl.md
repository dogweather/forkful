---
aliases:
- /zh/php/using-an-interactive-shell-repl/
date: 2024-01-26 04:16:24.411725-07:00
description: "\u4EA4\u4E92\u5F0Fshell\uFF0C\u6216\u79F0\u4E3AREPL\uFF08\u8BFB\u53D6\
  -\u6C42\u503C-\u6253\u5370\u5FAA\u73AF\uFF09\uFF0C\u5141\u8BB8\u60A8\u5373\u65F6\
  \u7F16\u5199\u548C\u8FD0\u884CPHP\u4EE3\u7801\u3002\u8FD9\u5BF9\u4E8E\u5B9E\u9A8C\
  \u3001\u8C03\u8BD5\u6216\u5B66\u4E60\u6765\u8BF4\u662F\u7406\u60F3\u7684\uFF0C\u56E0\
  \u4E3A\u60A8\u53EF\u4EE5\u6D4B\u8BD5\u4EE3\u7801\u7247\u6BB5\u800C\u65E0\u9700\u521B\
  \u5EFA\u5B8C\u6574\u811A\u672C\u7684\u5F00\u9500\u3002"
lastmod: 2024-02-18 23:08:59.217376
model: gpt-4-0125-preview
summary: "\u4EA4\u4E92\u5F0Fshell\uFF0C\u6216\u79F0\u4E3AREPL\uFF08\u8BFB\u53D6-\u6C42\
  \u503C-\u6253\u5370\u5FAA\u73AF\uFF09\uFF0C\u5141\u8BB8\u60A8\u5373\u65F6\u7F16\u5199\
  \u548C\u8FD0\u884CPHP\u4EE3\u7801\u3002\u8FD9\u5BF9\u4E8E\u5B9E\u9A8C\u3001\u8C03\
  \u8BD5\u6216\u5B66\u4E60\u6765\u8BF4\u662F\u7406\u60F3\u7684\uFF0C\u56E0\u4E3A\u60A8\
  \u53EF\u4EE5\u6D4B\u8BD5\u4EE3\u7801\u7247\u6BB5\u800C\u65E0\u9700\u521B\u5EFA\u5B8C\
  \u6574\u811A\u672C\u7684\u5F00\u9500\u3002"
title: "\u5728\u7F16\u7A0B\u4E2D\u4F7F\u7528\u4EA4\u4E92\u5F0FShell\uFF08REPL\uFF09"
---

{{< edit_this_page >}}

## 什么 & 为什么？
交互式shell，或称为REPL（读取-求值-打印循环），允许您即时编写和运行PHP代码。这对于实验、调试或学习来说是理想的，因为您可以测试代码片段而无需创建完整脚本的开销。

## 如何操作：
通过在终端运行`php -a`启动PHP REPL。下面是它的工作方式的一个示例：

```php
php > echo "Hello, World!";
Hello, World!
php > $arr = [1, 2, 3];
php > print_r($arr);
Array
(
    [0] => 1
    [1] => 2
    [2] => 3
)
```

您还可以定义函数：

```php
php > function sum($a, $b) { return $a + $b; }
php > echo sum(5, 10);
15
```

## 深入探讨
自1960年代LISP的早期以来，REPL以某种形式存在。与Python或JavaScript等语言相比，PHP的交互式shell不那么先进。它不会在会话之间保持状态，并缺少如自动完成等功能。对于一个功能更丰富的PHP REPL，考虑使用像`psysh`或`boris`这样的第三方shell。这些第三方shell提供更好的内省工具、标签完成以及甚至是一个调试器。

在底层，PHP的REPL通过编译和执行输入的每行代码来工作。这种方法的局限性在于像重新声明类这样的操作在同一会话中无法实现。这对于简单的测试很好，但对于复杂任务则可能变得繁琐。

## 另请参阅
- [PHP手册 - 交互式shell](https://www.php.net/manual/en/features.commandline.interactive.php)
- [PsySH：一个运行时开发者控制台，PHP的交互式调试器和REPL](https://psysh.org/)
- [Boris：一个微小的PHP REPL](https://github.com/borisrepl/boris)
