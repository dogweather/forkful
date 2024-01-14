---
title:    "PHP: 将字符串转换为小写"
keywords: ["PHP"]
---

{{< edit_this_page >}}

为什么：字符转换为小写的目的是为了统一数据，方便之后的处理。

如何做：在 ```PHP``` 代码块中展示具体的编码示例及输出结果。

```PHP
$string = "Hello World";
echo strtolower($string); // 输出结果为 "hello world"
```

深入了解：将字符串转换为小写的原理是调用 ```strtolower()``` 函数，该函数会将字符串中所有的大写字母转换成小写字母。这样可以避免因大小写不统一而导致的数据处理错误。

另外，还有一些相关的函数可用来转换字符串大小写，比如 ```strtoupper()``` 可以将字符串中的所有小写字母转换成大写字母，而 ```ucwords()``` 则可以将字符串中每个单词的首字母转换成大写字母。根据实际需求，选择合适的函数进行字符串大小写转换。

还有一点需要注意的是，某些语言中的字符可能存在特殊情况，如 Türkçe 中的 ```i``` 字母，在转换为小写时会变成 ```ı```，而不是 ```i```。因此，在开发过程中需重点关注不同语言下的字符处理。

另外，如果想要转换的字符串中包含数字和特殊符号，可以使用正则表达式结合 ```strtolower()``` 函数来实现转换。

参考链接：[PHP 全部字符串小写/大写/首字母大写转换方法总结](http://www.zhouyunlong.com/1636.html)

还有：以 ```mb_strtolower()``` 为例，深入了解 PHP 中字符编码的相关知识，以及如何处理 Unicode 字符串。

参考链接：[mbstring 编程参考手册](https://www.php.net/manual/zh/ref.mbstring.php)

请查看：[相关的字符串函数概述](https://www.php.net/manual/zh/ref.strings.php)