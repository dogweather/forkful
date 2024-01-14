---
title:                "PHP: 写入标准错误"
simple_title:         "写入标准错误"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

为什么：写入标准错误是一个有用的调试工具，它允许开发者在调试代码时捕获程序中的错误和警告消息。

如何做：下面是一些示例代码和输出，展示如何将错误消息写入标准错误。

```PHP
<?php
// 引发一个警告消息
trigger_error("这是一个警告消息！", E_USER_WARNING);

// 引发一个错误消息
trigger_error("这是一个错误消息！", E_USER_ERROR);

// 将自定义错误消息写入标准错误
$error_message = "这是一个自定义的错误消息！";
fwrite(STDERR, $error_message);
?>
```

输出：

```
警告：这是一个警告消息！
```
```
致命错误：这是一个错误消息！
```
```
这是一个自定义的错误消息！
```

深入了解：通过写入标准错误，开发者可以在编写代码时即时捕获和调试错误消息，而无需在浏览器上查看网页源代码。此外，它还可以帮助开发者避免在生产环境中显示敏感的错误消息，从而提高网站的安全性。

另外，通过使用`fwrite()`函数，开发者可以控制错误消息的格式和内容，从而更有效地调试代码。可以在[PHP错误处理官方文档](https://www.php.net/manual/en/function.trigger-error.php)中找到更多有用的信息。

## 参考文献

* [PHP fwrite()函数官方文档](https://www.php.net/manual/en/function.fwrite.php)
* [PHP错误处理官方文档](https://www.php.net/manual/en/function.trigger-error.php)
* [如何有效地调试PHP代码](https://www.geeksforgeeks.org/how-to-efficiently-debug-php-code/)
* [深入理解标准错误](https://www.oreilly.com/library/view/php-cookbook/0596006811/ch13s11.html)

# 另请参阅

* [PHP调试技巧](https://medium.com/@popcoder/debugging-php-a6bda4fcfb36) 
* [如何在PHP中使用日志来调试代码](https://dzone.com/articles/a-simpler-way-of-debugging-php-cod) 
* [使用Xdebug进行PHP调试](https://www.hostinger.com/tutorials/debugging-php)