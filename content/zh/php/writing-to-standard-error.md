---
title:    "PHP: 写入标准错误"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 为什么

在编程中，有时候我们需要将一些重要的信息发送到标准错误流，这对于调试程序或者处理异常情况非常有用。因此，了解如何将信息写入到标准错误是一个必不可少的技能。

## 如何进行操作

为了写入标准错误流，我们可以使用PHP中的fwrite()函数。下面是一个简单的例子，展示如何将一段文本写入到标准错误流：

```PHP
fwrite(STDERR, "这是一个测试错误信息");
```

运行这段代码后，我们将在标准错误中看到类似于以下的输出：

```
这是一个测试错误信息
```

## 深入了解

除了fwrite()函数之外，我们也可以使用其他的方式来写入到标准错误流。例如，可以使用PHP的error_log()函数来记录特定的错误信息，或者使用trigger_error()函数来手动触发一个错误并将其写入到标准错误流中。

此外，我们也可以通过重定向标准错误流来将错误信息输出到指定的日志文件中。这样做可以让我们更方便地查看和管理错误信息。

## 参考链接

- PHP官方文档：<https://www.php.net/manual/zh/function.fwrite.php>
- fwrite()函数的使用方法：<https://www.w3schools.com/php/func_filesystem_fwrite.asp>
- 使用error_log()函数记录错误信息：<https://www.php.net/manual/zh/function.error-log.php>## 参见