---
title:                "使用正则表达式"
html_title:           "PHP: 使用正则表达式"
simple_title:         "使用正则表达式"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/using-regular-expressions.md"
---

{{< edit_this_page >}}

什么是正则表达式，为什么程序员要用它？

正则表达式是一种强大的文本匹配工具，它可以帮助程序员快速有效地搜索和处理文本数据。程序员使用正则表达式来执行各种任务，比如验证输入数据、提取信息和替换文本。

如何使用：

使用PHP内置的preg_match（）函数可以进行正则表达式匹配。在括号中，第一个参数是需要匹配的正则表达式模式，第二个参数是要搜索的文本，第三个参数是一个变量，用于存储匹配的结果。

```
$pattern = '/hello/';
$text = 'Hello world';
preg_match($pattern, $text, $matches);

print_r($matches);
// output: Array ( [0] => Hello )
```

深入了解：

正则表达式最早是由贝尔实验室的计算机科学家Ken Thompson和Rob Pike发明的。它们被用于早期的UNIX操作系统中，用来搜索和替换文本。

除了PHP，其他编程语言也支持正则表达式，如Perl、Python和Java。每种语言都有不同的语法和特性，但原理都是相同的。

在实现正则表达式时，通常会使用有限状态自动机（Finite-state Machine）来处理原始文本和正则表达式。这样可以实现高效的匹配算法，因此正则表达式可以处理大量的文本数据。

相关资源：

- PHP官方文档：https://www.php.net/manual/en/function.preg-match.php
- Perl官方文档：https://perldoc.perl.org/perlre.html
- Python官方文档：https://docs.python.org/3/library/re.html
- Java官方文档：https://docs.oracle.com/javase/tutorial/essential/regex/