---
title:                "PHP: 连接字符串"
simple_title:         "连接字符串"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/concatenating-strings.md"
---

{{< edit_this_page >}}

## 为什么
在编程中，经常会遇到需要将多个字符串连接在一起的情况。这种操作被称为字符串拼接，它可以帮助我们动态的生成字符串，使得代码更加灵活和高效。

## 如何做
要使用PHP进行字符串拼接，我们可以使用“.”（点号）或者“.=”（点等号）来连接多个字符串。例如：

```PHP
// 使用.进行字符串拼接
$name = "张三";
$greeting = "你好" . $name;
echo $greeting;   // 输出：你好张三

// 使用.=进行字符串拼接
$message = "今天是";
$message .= "星期一";
echo $message;    // 输出：今天是星期一
```

除了连接变量中的字符串，我们也可以直接在拼接过程中加入文本。例如：

```PHP
// 直接在拼接过程中加入文本
$message = "现在是" . date('H:i') . ", " . "外面" . "下雨了！";
echo $message;    // 输出：现在是15:30, 外面下雨了！
```

## 深入了解
在PHP中，字符串拼接实际上是将多个字符串连接成一个整体的操作。PHP中的字符串拼接使用的是“字节数组”（byte array）的方法，也就是将每个字符串的字节按照顺序拼接在一起，形成一个新的字符串。因此，无论是使用“.”还是“.=”进行拼接，实际上都会创建一个新的字符串。

此外，PHP中还有一个函数`implode()`可以用于拼接数组中的元素，返回一个字符串。它的使用方法如下：

```PHP
$names = array("张三", "李四", "王五");
$string = implode("、", $names);
echo $string;   // 输出：张三、李四、王五
```

## 参考资料
- [PHP 字符串拼接](https://www.runoob.com/php/php-strings.html)
- [PHP implode函数](https://www.php.net/manual/zh/function.implode.php)

## 参见
- [PHP 字符串相关操作](https://github.com/username/example)
- [PHP 数组操作](https://github.com/username/example)