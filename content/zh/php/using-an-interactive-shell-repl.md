---
title:                "在编程中使用交互式Shell（REPL）"
date:                  2024-01-26T04:16:24.411725-07:00
model:                 gpt-4-0125-preview
simple_title:         "在编程中使用交互式Shell（REPL）"

category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/using-an-interactive-shell-repl.md"
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
