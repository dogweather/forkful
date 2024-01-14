---
title:                "PHP: 创建临时文件"
simple_title:         "创建临时文件"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

为什么：为什么有人会选择创建临时文件？

创建临时文件在PHP编程中是一项非常有用的技术。它可以让程序员临时存储数据或执行操作，而无需永久改变或污染服务器上的其他文件。这对于测试和调试代码尤其有用。

## 如何创建临时文件

在PHP中，我们可以使用`tempnam()`函数来创建临时文件。这个函数接受两个参数，第一个参数是指定临时文件的目录，第二个参数是可选的前缀。例如，我们可以使用以下代码来创建一个名为“tempfile”的临时文件：

```PHP
$temp_dir = "/tmp/";
$file_name = tempnam($temp_dir, "tempfile");
```

当代码执行完后，我们可以在指定的目录中找到名为“tempfile”的临时文件。我们还可以使用`tempnam()`函数来指定一个空字符串作为前缀，从而让系统为我们生成一个随机的文件名。

## 深入了解创建临时文件

创建临时文件涉及到很多细节，比如文件命名的算法、临时文件的创建位置和权限设置。在PHP中，我们可以通过设置`sys_temp_dir`选项来指定临时文件的默认创建目录。当然，我们也可以使用`tempnam()`函数来自己指定创建目录。

此外，我们还需要考虑文件的权限设置。临时文件应该具有足够的权限，能够让PHP程序在其上执行相关操作。一般来说，我们可以使用`chmod()`函数来设置文件的权限。

临时文件也需要正确地处理，即当程序结束时，它们应该被自动删除。在PHP中，我们可以使用`register_shutdown_function()`函数来注册一个函数，用于在程序结束时清理临时文件。

## 参考链接

- [PHP官方文档-`tempnam()`函数](https://www.php.net/manual/zh/function.tempnam.php)
- [How to Create Temporary Files in PHP](https://www.geeksforgeeks.org/how-to-create-temporary-file-in-php/)
- [官方PHP源码中`tempnam()`函数的实现](https://github.com/php/php-src/blob/master/ext/standard/file.c#L711)

## 请参阅

- [为什么使用临时文件](https://geekflare.com/php-create-temporary-file/)
- [PHP官方文档-`chmod()`函数](https://www.php.net/manual/zh/function.chmod.php)
- [官方PHP源码中`register_shutdown_function()`函数的实现](https://github.com/php/php-src/blob/master/main/php_shutdown.c#L474)