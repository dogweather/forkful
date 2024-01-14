---
title:                "PHP: 打印调试输出"
programming_language: "PHP"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/printing-debug-output.md"
---

{{< edit_this_page >}}

# 为什么要输出调试信息

输出调试信息是一个在编写PHP程序中非常有用的技巧。它可以帮助程序员快速定位问题并进行调试，节省大量的时间和精力。

## 如何输出调试信息

要输出调试信息，可以使用PHP语言内置的函数`print_r()`或者`var_dump()`。这两个函数可以将变量的值以易于阅读的形式打印出来。

```PHP
// 使用print_r()输出数组
$array = ["苹果", "橘子", "香蕉"];
print_r($array);

// 使用var_dump()输出字符串
$string = "这是一段字符串";
var_dump($string);
```

运行以上代码，将得到如下输出：

```
Array
(
    [0] => 苹果
    [1] => 橘子
    [2] => 香蕉
)
string(15) "这是一段字符串"
```

除了这两个函数外，还可以使用`error_log()`函数将调试信息输出到服务器的错误日志中，方便在生产环境中进行调试。

## 深入探讨

输出调试信息可以帮助程序员了解程序在执行过程中变量的具体值，从而分析程序运行过程中的错误。尤其在排查复杂的bug时，输出调试信息会起到至关重要的作用。

不过，需要注意的是，输出调试信息时要小心使用，避免将敏感信息暴露给未授权的用户。因此，在生产环境中需要将调试信息关闭或者限制输出内容，避免造成安全漏洞。

# 查看更多

想要了解更多关于PHP调试技巧的信息，请参考以下链接：

- [PHP官方文档](https://www.php.net/manual/en/function.print-r.php)
- [调试PHP应用程序](https://www.sitepoint.com/debug-php-application-5-strategies/)
- [PHP调试工具推荐](https://medium.com/@dkhd/mastering-php-debugging-tools-763a18c0e9a8)

祝您编写愉快！