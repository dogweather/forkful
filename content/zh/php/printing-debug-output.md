---
title:                "打印调试输出"
html_title:           "Clojure: 打印调试输出"
simple_title:         "打印调试输出"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/printing-debug-output.md"
---

{{< edit_this_page >}}

# "PHP 中的调试输出-如何以及为何？"
要理解程序是否运行到预期请查看输出，无论是收集错误信息，还是了解程序的运行过程，调试输出都是重要工具。

首先，让我们来看看是什么以及为何要调试输出。然后，我们将深入了解一下更多的细节。

## 什么是，以及为何？
调试输出是在运行时记录信息的方式，比如程序的状态或变量的值。这有助于理解并修复代码中的问题。

## 如何实现：
在PHP中打印调试输出，你可以使用 `echo`, `print`, `print_r`, `var_dump` 或者 `var_export` 。以下是几个例子：
```PHP
<?php
// 使用 echo
echo "这是一个调试信息";

// 使用 print
print "这是一个调试信息";

// 使用 print_r
print_r(["apple", "banana", "cherry"]);

// 使用 var_dump 
$fruit = array("apple", "banana", "cherry");
var_dump($fruit);

// 使用 var_export 
$fruit = array("apple", "banana", "cherry");
var_export($fruit);
?>
```
当你运行这段代码，你会在浏览器中看到相关的输出信息。

## 深入探讨
调试输出在早期的编程中已经非常重要。虽然现代的IDE和调试器提供了更为复杂的调试方法，但简单的调试输出仍然是一个重要的工具。

虽然上述所有函数都可以用来输出调试信息，但它们之间还是有一些不同。`echo` 和 `print` 只能输出简单的字符串，而 `print_r`, `var_dump` 和 `var_export` 可以输出更为复杂的数据结构，例如数组和对象。

使用 `var_dump` 或 `var_export` 可以给出更详细的信息，例如类型和值。`var_export` 的另一个优点是它返回的字符串可以用 `eval()` 函数执行，从而重新生成原来的值。

## 需要更多信息？
要了解PHP调试输出的更多内容，请查看以下资源：
* PHP 官方在线文档 <https://www.php.net/>
* PHP The Right Way <https://phptherightway.com/>
* Stack Overflow <https://stackoverflow.com/>

お忍びください。是时候去调试你的代码了！