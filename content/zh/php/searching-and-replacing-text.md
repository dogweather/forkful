---
title:                "搜索和替换文本"
html_title:           "PHP: 搜索和替换文本"
simple_title:         "搜索和替换文本"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# 什么是搜索和替换文本？为什么程序员要做这个？

搜索和替换文本是指在文本中查找指定的内容，并将其替换为另一种内容。这项任务在编程中非常常见，因为它可以帮助程序员快速地修改大段的代码或文本，从而节省时间和精力。

# 如何完成搜索和替换文本？

在PHP中，有两个主要的函数可以用来完成搜索和替换文本的任务：`str_replace()`和`preg_replace()`。

```PHP
// 使用str_replace()函数来替换文本
$text = "Hello World!";
$new_text = str_replace("World", "PHP", $text);
echo $new_text; // 输出：Hello PHP!

// 使用preg_replace()函数来替换文本
$pattern = "/worl/i"; // i标志表示不区分大小写
$text = "Hello World!";
$new_text = preg_replace($pattern, "PHP", $text);
echo $new_text; // 输出：Hello PHP!
```

另外，可以使用正则表达式作为搜索的模式，从而更灵活地匹配和替换内容。

# 深入了解搜索和替换文本

搜索和替换文本的概念最早出现在编辑器和文本处理器中，让用户可以批量修改文档中的文本。如今，这项技术也被广泛应用于编程领域。

除了`str_replace()`和`preg_replace()`函数，PHP也提供了其他一些相关的函数，如`str_ireplace()`（不区分大小写的搜索和替换）和`mb_eregi_replace()`（支持多字节字符的搜索和替换）。

## 相关资料

- [PHP官方文档 - str_replace()](https://www.php.net/manual/en/function.str-replace.php)
- [PHP官方文档 - preg_replace()](https://www.php.net/manual/en/function.preg-replace.php)
- [PHP官方文档 - str_ireplace()](https://www.php.net/manual/en/function.str-ireplace.php)
- [PHP官方文档 - mb_eregi_replace()](https://www.php.net/manual/en/function.mb-eregi-replace.php)