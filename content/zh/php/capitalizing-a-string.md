---
title:                "PHP: 将字符串首字母大写"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

##为什么要对字符串进行大写处理？

对于任何一种编程语言，字符串都是一种基本的数据类型。它们用来存储和处理文本信息，如用户输入、文件内容等。有时候，我们需要对字符串中的某些字符进行大小写转换，以满足特定的要求或方便数据处理。这时，大写处理就非常重要，它可以让我们快速地将字符串中的字符转换为大写形式，从而节省时间和精力。

##如何进行字符串的大写处理？

使用PHP编程语言进行字符串的大写处理非常简单，只需要使用内置函数`strtoupper()`即可。下面是一个示例代码：

```PHP
$string = "hello world";
$new_string = strtoupper($string);
echo $new_string;
```

输出结果为：HELLO WORLD。

可以看到，原本的字符串中的所有字符都被转换为大写形式。在实际应用中，我们也可以对用户输入的数据进行大写处理，从而保证数据的统一性。

##深入了解字符串的大写处理

字符串的大写处理过程实际上是将每个字符的ASCII码值减去32，从而得到它的大写形式。这也是为什么大写处理后，字符串的长度与原始字符串相同的原因。同时，PHP还提供了另一个内置函数`mb_strtoupper()`，它可以处理包含多字节字符的情况。

##参考链接

- [PHP中的strtoupper()函数](https://www.php.net/manual/en/function.strtoupper.php) 
- [PHP中的mb_strtoupper()函数](https://www.php.net/manual/en/function.mb-strtoupper.php)