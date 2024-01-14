---
title:    "PHP: 读取命令行参数"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 为什么读取命令行参数

读取命令行参数是一项非常有用的技能，它可以帮助您编写更强大的PHP程序。通过读取命令行参数，您可以使您的程序更加灵活和可配置，从而为用户提供更好的体验。 

## 如何读取命令行参数

要在PHP中读取命令行参数，您可以使用 `$argv` 全局变量。该变量将保存命令行参数的值作为数组。让我们来看一个例子：

```PHP
//在命令行中调用： php myscript.php arg1 arg2
<?php
//读取第一个参数
$arg1 = $argv[1];
echo "第一个参数是：" . $arg1 . "\n";

//读取第二个参数
$arg2 = $argv[2];
echo "第二个参数是：" . $arg2 . "\n";
?>
```

输出将会是：

```
第一个参数是：arg1
第二个参数是：arg2
```

您还可以使用 `count()` 函数来获取传递给脚本的参数数量，从而使您的代码更加灵活。

## 深入了解读取命令行参数

除了使用 `$argv` 变量之外，您还可以使用 `getopt()` 函数来读取命令行参数。此函数接受三个参数：options（选项），longopts（长选项）和optind（索引）。options参数用于指定单个字母的选项，而longopts参数用于指定长的选项。让我们来看一个例子：

```PHP
//在命令行中调用： php myscript.php -a -b "hello"
<?php
$options = getopt("ab:");
//读取选项a
if (isset($options["a"])) {
    echo "-a 选项已被设置\n";
}

//读取选项b
if (isset($options["b"])) {
    $b = $options["b"];
    echo "-b 选项的值是：" . $b . "\n";
}
?>
```

输出将会是：

```
-a 选项已经被设置
-b 选项的值是：hello
```

要深入了解getopt()函数的使用，请查看PHP官方文档。

## 查看也门

- [PHP全局变量 - $argv](https://www.php.net/manual/zh/reserved.variables.argv.php)
- [getopt()函数](https://www.php.net/manual/zh/function.getopt.php)
- [PHP脚本参数教程](https://www.w3schools.com/php/php_cli.asp)

谢谢您阅读本文，希望您能在将来的PHP编程中受益！