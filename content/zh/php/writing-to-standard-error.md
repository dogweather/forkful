---
title:                "写入标准错误"
html_title:           "PHP: 写入标准错误"
simple_title:         "写入标准错误"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 为什么？

程序员经常需要输出错误信息来帮助调试代码。通过将错误信息写入标准错误流，可以方便地在命令行界面或日志文件中查看，从而更快地定位和解决问题。

## 如何做？

在PHP中，可以使用`fwrite()`函数来写入标准错误流。首先需要获取标准错误流的句柄，并指定写入的信息。以下是一个例子：

```PHP
$stderr = fopen('php://stderr', 'w'); //打开标准错误流句柄
fwrite($stderr, "This is an error message."); //写入错误信息
fclose($stderr); //关闭句柄
```

运行以上代码，并将错误信息写入标准错误流。在命令行界面中，可以通过`php yourfile.php 2>/dev/null`来运行代码，并将错误信息输出到空设备中。如果需要将错误信息写入日志文件，可以使用PHP内置的`error_log()`函数。

## 深入了解

除了在调试代码时输出错误信息，还可以通过将错误信息写入标准错误流来实现实时的日志记录。这对于大型的应用程序非常有用，可以帮助排查生产环境中的错误。同时，写入标准错误流的信息可以被其他命令行工具所读取和处理，为程序员提供更多选择。

## 参考链接

- [PHP fwrite()函数文档](https://www.php.net/manual/zh/function.fwrite.php)
- [PHP fopen()函数文档](https://www.php.net/manual/zh/function.fopen.php)
- [PHP error_log()函数文档](https://www.php.net/manual/zh/function.error-log.php)
- [如何在PHP中记录实时日志信息？](https://stackoverflow.com/questions/8313226/how-to-log-real-time-in-php)