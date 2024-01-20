---
title:                "将字符串转换为小写"
html_title:           "Arduino: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 什么是字符串转换为小写？为什么要这么做？

字符串转换为小写即将字符串中的所有大写字母转换为小写。程序员通常这样做是为了比较，排序和搜索等需要字符串大小写一致的操作。

## 如何操作：

我们可以使用PHP中的 `strtolower()` 函数来将字符串转换为小写。看代码例子：

```PHP
<?php
$str = "Hello World!";
echo strtolower($str);
?>
```

这段代码的输出会是：

```PHP
"hello world!"
```

## 深度解析

在 PHP 的历史发展中，`strtolower()` 一直是用来转换字符串为小写的标准方式。它具有可靠性和简洁性，使程序员们依赖于它。

如果你需要替代方案，`mb_strtolower()` 是一个可以考虑的函数。它是多字节字符串函数，可以处理包括非拉丁字符在内的更广泛的字符集。优点在于它支持国际化，但是它要求安装并启动 mbstring 扩展库：

```PHP
<?php
$str = "Hello World!";
echo mb_strtolower($str);
?>
```

以 `strtolower()` 函数为例，其具体实现细节包括读取字符串中的每一个字符，检测它们是否为大写，如果是，则替换为对应的小写字符。

## 参考信息

- PHP官方文档中的 `strtolower()` 函数页面: http://php.net/manual/en/function.strtolower.php
- `mb_strtolower()` 函数在 PHP 文档中的页面： http://php.net/manual/en/function.mb-strtolower.php
- 在 StackOverflow 上关于 `strtolower()` 和 `mb_strtolower()` 的讨论： https://stackoverflow.com/questions/1725252/php-strtoupper-vs-mb-strtoupper