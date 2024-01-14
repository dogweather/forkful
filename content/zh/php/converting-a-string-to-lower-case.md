---
title:    "PHP: 转换一个字符串到小写"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

为什么：在编程时，我们经常需要处理字符串。将字符串转换为小写可能是必需的，因为它可以让我们在比较和搜索字符串时更方便、更准确。

如何做到：要将字符串转换为小写，我们可以使用PHP内置的strtolower（）函数。它将返回一个所有字母都变成小写的新字符串。让我们来看一个简单的例子：

```PHP
$string = "Hello WORLD";
echo strtolower($string);
```

输出将会是“hello world”。可以看到，原来的大写字母已经全部变成了小写。这对于需要在用户输入或数据库中搜索字符串时非常有用。

深入探讨：想要深入了解字符串的转换，我们需要了解一些基础知识。在计算机中，每个字符都有一个对应的ASCII码。大写字母和小写字母有不同的ASCII码，因此我们可以通过对比ASCII码来实现转换。但是，PHP的strtolower（）函数会自动处理这一过程，让我们更方便地转换字符串。

此外，还有一些其他的字符串转换函数，比如ucfirst（）可以将字符串的首字母变成大写，ucwords（）可以将每个单词的首字母都变成大写。这些函数也都是应用对应的ASCII码规则来实现字符串的转换。

请注意，由于不同语言的字符集不同，有些特殊的字符在转换中可能会产生意想不到的结果。因此，在编程中一定要注意字符集的差异性。

See Also（参考链接）：
- https://www.php.net/manual/en/function.strtolower.php
- https://www.php.net/manual/en/function.ucfirst.php
- https://www.php.net/manual/en/function.ucwords.php
- https://en.wikipedia.org/wiki/ASCII