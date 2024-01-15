---
title:                "将字符串转换为小写"
html_title:           "PHP: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 为什么

如果你使用PHP写程序，你可能会遇到需要将字符串转换为小写字母的情况。这可能是因为你想进行字符串匹配，或者您想要在输出时统一格式。

## 怎样做

```PHP
$string = "THIS IS A STRING";

echo strtolower($string);

// Output: this is a string
```

使用PHP内置的`strtolower()`函数可以轻松地将字符串转换为小写。该函数接受一个字符串作为参数，并返回一个新的字符串，该字符串中所有的字符都被转换为小写。通过这样做，您可以方便地比较字符串，或者在搜索和输出时统一格式。

### 更多选项

除了`strtolower()`函数外，PHP还提供了其他一些函数来转换字符串的大小写。

- `strtoupper()`：将字符串中所有字符转换为大写
- `ucfirst()`：将字符串的第一个字符转换为大写
- `ucwords()`：将字符串中每个单词的首字母转换为大写

## 深入探讨

虽然转换字符串大小写可能看起来很简单，但实际上有很多细节需要考虑。例如，某些字符在不同语言中可能具有不同的大小写形式，因此需要谨慎处理。此外，一些特殊字符可能不会按照您预期的方式转换。因此，在使用字符串转换函数时，一定要仔细检查输出，以确保它们符合您的预期。

## 参考资料

- PHP官方文档：https://www.php.net/manual/zh/function.strtolower.php
- PHP字符串函数：https://www.php.net/manual/zh/ref.strings.php

## 参见

- "PHP 字符串函数简介"：https://www.php.net/manual/zh/ref.strings.php
- "PHP字符串处理技巧"：https://www.php.net/manual/zh/language.types.string.php#language.types.string.conversion