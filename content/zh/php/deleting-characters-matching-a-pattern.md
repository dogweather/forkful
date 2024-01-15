---
title:                "删除与模式匹配的字符"
html_title:           "PHP: 删除与模式匹配的字符"
simple_title:         "删除与模式匹配的字符"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why

为什么要删除匹配某种模式的字符？通常情况下，我们会通过字符串操作来对文本进行处理，而删除字符可以帮助我们快速而有效地清理和筛选出所需内容。

## How To

首先，我们需要使用PHP内置的str_replace函数来删除指定的字符。比如，我们想删除字符串中所有的空格，代码如下所示：

```PHP
<?php
    $str = "Hello, World!";
    $str = str_replace(" ", "", $str);
    echo $str;
?>
```

输出将会是"Hello,World!"，空格已经被删除成功。

除了单个字符，我们也可以删除字符串中符合某种模式的字符。比如，我们想要删除所有的数字，代码如下所示：

```PHP
<?php
    $str = "I have 5 dogs and 3 cats.";
    $str = preg_replace('/[0-9]/', '', $str);
    echo $str;
?>
```

输出将会是"I have dogs and cats."，所有数字已经被成功删除。

## Deep Dive

在深入了解删除字符匹配模式之前，我们先来了解一下正则表达式。正则表达式是一种用来筛选和匹配字符模式的工具，我们可以利用它来删除指定的字符。使用PHP的preg_replace函数，我们可以在正则表达式中使用特殊符号来匹配不同的字符或字符串。

例如，要删除所有的标点符号，我们可以使用正则表达式"[:punct:]"，代码如下所示：

```PHP
<?php
    $str = "Hello, World! How are you?";
    $str = preg_replace("/[:punct:]/", "", $str);
    echo $str;
?>
```

输出将会是"Hello World How are you"，所有标点符号已经被成功删除。正则表达式中还有很多其他的特殊符号，可以根据需要来使用。

## See Also

- PHP官方文档：https://www.php.net/manual/en/function.str-replace.php
- PHP正则表达式教程：https://www.php.net/manual/en/reference.pcre.pattern.syntax.php
- 正则表达式测试工具：https://regex101.com/