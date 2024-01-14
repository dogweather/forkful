---
title:                "PHP: 串接字符串"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/concatenating-strings.md"
---

{{< edit_this_page >}}

##为什么？

在编程中，拼接字符串是一种常见的操作，它可以将多个小的字符串连接起来形成一个更长的字符串。这样做的主要原因是为了方便数据的处理和展示。举个例子，当我们需要在网页上显示某个用户的完整信息时，就需要将其姓名、电话号码和地址等信息拼接成一个完整的字符串。

##如何做？

在PHP中，我们可以使用“.”符号来拼接字符串。下面是一个简单的代码示例：

```PHP
$name = "张三";
$phone = "123456789";
$address = "北京市";
$userInfo = $name . "的联系方式为：" . $phone . "，地址为：" . $address;
echo $userInfo;
```

运行上面的代码，我们可以得到如下输出：

> 张三的联系方式为：123456789，地址为：北京市

在这个例子中，我们将三个变量分别包含了姓名、电话号码和地址的信息，然后通过使用“.”符号将它们连接起来，最终得到了完整的用户信息字符串。

##深入解析

拼接字符串在PHP中的实现原理是通过使用内部的字符串操作函数`concat()`来完成的。当我们使用“.”符号拼接字符串时，实际上就是调用了这个函数。除此之外，PHP还提供了其他一些字符串操作函数，如`str_replace()`、`substr()`等等，可以帮助我们更方便地处理字符串。

##参考链接

- [PHP官方手册-字符串](https://www.php.net/manual/zh/language.types.string.php)
- [PHP字符串操作函数参考](https://www.w3schools.com/php/php_ref_string.asp)
- [PHP字符串拼接技巧](https://www.runoob.com/php/php-string-concat.html)

##请参阅

- [PHP基础知识教程](https://zh.wikipedia.org/zh-hans/PHP)
- [PHP变量和数据类型](https://www.php.net/manual/zh/language.types.php)
- [PHP字符串函数完整列表](https://www.php.net/manual/zh/ref.strings.php)
- [PHP字符串格式化](https://www.php.net/manual/zh/function.sprintf.php)