---
title:                "写给标准错误"
html_title:           "PHP: 写给标准错误"
simple_title:         "写给标准错误"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 什么是writing to standard error？为什么程序员要这么做？

Writing to standard error是指在程序中将错误信息打印到控制台。程序员经常这样做是为了快速检测和解决程序中的错误，使得程序更加稳定和可靠。

## 如何实现？

在PHP中，可以通过使用`error_log()`函数将错误信息打印到标准错误流。下面是一个简单的例子：

```PHP
$num = 10 / 0;
if (!$num) {
  error_log("除数不能为0！");
}
```

执行以上代码后，控制台会输出错误信息“除数不能为0！”。除了使用`error_log()`函数，还可以使用`trigger_error()`函数和`die()`函数来输出错误信息。

## 深入了解

写入标准错误流是一种用于调试和错误处理的常用方法，可以帮助程序员快速定位错误。除了PHP，其他编程语言如Java和Python也都提供了类似的功能。

除了将错误信息打印到控制台，还可以将其记录到日志文件中，方便后续分析和修复。此外，也可以使用调试工具来捕获和查看程序中的错误。

## 参考资料

- PHP中文手册：error_log - http://php.net/manual/zh/function.error-log.php
- PHP中文手册：trigger_error - http://php.net/manual/zh/function.trigger-error.php
- PHP中文手册：die - http://php.net/manual/zh/function.die.php