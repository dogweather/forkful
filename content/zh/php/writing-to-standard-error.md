---
title:                "PHP: 向标准错误写入"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 为什么

作为一个程序员，写入标准错误可能是一个非常普遍的任务。当你的代码遇到错误时，它可能会在屏幕上输出一些错误信息，这些信息通常都是写入标准错误的。所以，学习如何写入标准错误是非常重要的，它可以帮助我们更好地调试我们的代码，找出问题所在。

## 如何做

要在PHP中写入标准错误，我们使用 `error_log()` 函数。它有两个必需的参数，第一个参数是要写入的错误消息，第二个参数是一个表示错误消息级别的整数值。常用的级别有：`0` - 默认值，将错误信息写入 PHP 错误日志；`1` - 将错误信息发送到指定的邮件地址；`3` - 将错误信息写入标准错误。

让我们来看一个例子：

```PHP
<?php
if (!file_exists("example.php")) {
  $error_msg = "文件不存在！";
  error_log($error_msg, 3);
  echo "文件不存在，错误消息已写入标准错误。";
}
?>
```

上面的代码将检查 `example.php` 是否存在，如果不存在，则会在标准错误中写入一个错误消息，并将其打印到页面上。请注意，我们在 `error_log()` 中指定了级别为 `3`，这将把错误消息写入标准错误。

现在，让我们来运行一下，看看会发生什么。如果 `example.php` 存在，则会得到以下输出：

```
文件不存在，错误消息已写入标准错误。
```

同时，在 PHP 错误日志中也会写入错误消息。如果 `example.php` 不存在，则会得到以下输出：

```
文件不存在！
```

同时，在标准错误中也会写入错误消息。

## 深入探讨

通过使用 `error_log()` 函数，我们可以将错误消息发送到多种地方，包括写入 PHP 错误日志和发送到指定的邮件地址。此外，我们还可以根据不同的错误类型和级别来定制我们的错误消息。如果想要了解更多关于错误日志的内容，请查阅 [PHP 手册](https://www.php.net/manual/zh/function.error-log.php)。

## 另请参阅

- [PHP 手册 - error_log()](https://www.php.net/manual/zh/function.error-log.php)
- [PHP 手册 - 错误处理文档](https://www.php.net/manual/zh/book.errorfunc.php)