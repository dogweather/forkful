---
title:                "提取子字符串"
html_title:           "PHP: 提取子字符串"
simple_title:         "提取子字符串"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## 为什么

让我们来谈谈提取子字符串的激动人心的原因。在编程过程中，我们经常需要处理大量的文本数据。提取子字符串是一种简单而有效的方法，可以帮助我们从文本中找到我们需要的特定信息。不仅如此，提取子字符串还可以帮助我们进行数据清洗和格式化，使得我们的代码更加健壮和易读。

## 如何

为了提取子字符串，我们可以使用PHP内置的substr()函数。在其中，我们需要传入三个参数：原始字符串、起始位置和长度。让我们来看一个具体的例子：

```PHP
// 原始字符串
$string = "Hello World";

// 提取 "World" 子字符串
$substring = substr($string, 6, 5);

// 输出: World
echo $substring;
```

现在我们可以看到，通过指定起始位置和长度，我们成功地提取了我们需要的子字符串。当然，我们也可以通过使用负数作为起始位置来从字符串末尾开始提取子字符串。让我们来看另一个例子：

```PHP
// 原始字符串
$string = "Hello World";

// 提取 "lo World" 子字符串
$substring = substr($string, 3, -1);

// 输出: lo World
echo $substring;
```

总的来说，使用substr()函数，我们可以通过简单的方式提取出我们需要的子字符串，并且可以根据实际需要来进行格式调整。

## 深入了解

除了使用substr()函数，我们还可以使用其他函数来提取子字符串，比如mb_substr()函数可以处理多字节字符。此外，我们还可以使用正则表达式来实现更加复杂的子字符串提取逻辑。如果您想要深入了解更多关于提取子字符串的知识，可以参考下面的资源。

## 参考资料

- [PHP官方手册：substr()函数](https://www.php.net/manual/en/function.substr.php)
- [PHP官方手册：mb_substr()函数](https://www.php.net/manual/en/function.mb-substr.php)
- [PHP正则表达式教程](https://www.php.net/manual/en/reference.pcre.pattern.syntax.php)
- [PHP正则表达式在线测试工具](https://regex101.com/)