---
title:    "PHP: 写入标准错误"
keywords: ["PHP"]
---

{{< edit_this_page >}}

##为什么要写标准错误

标准错误是用于记录程序中出现错误或异常的重要工具。通过将错误信息写入标准错误流，程序员可以更轻松地跟踪和调试问题，并及时修复代码中的错误。因此，写入标准错误是一种有效的调试技术，可以帮助程序员提高代码质量。

##如何写入标准错误

要将错误信息写入标准错误流，我们可以使用PHP中的"```PHP error_log() ```"函数。下面是一个简单的示例，演示如何使用此函数将错误信息写入标准错误：

```PHP
<?php
$error_message = "An error occurred!";
error_log($error_message);
?>
```

运行此代码后，错误信息将被写入标准错误流，并且可以在适当的地方被显示出来。下面是一个示例输出：

```PHP
[Mon Aug 23 16:03:15 2021] [error] [client 127.0.0.1] An error occurred!
```

通过这种方式，我们可以在程序中任何需要的地方将错误信息写入标准错误流，并通过查看输出来定位问题所在。

##深入了解写入标准错误

除了使用"```PHP error_log() ```"函数外，我们还可以使用其他方法来写入标准错误。例如，我们可以将错误信息输出到屏幕而不是标准错误流，以便可视化查看。我们还可以设置标准错误的格式，并将其写入日志文件中，以便更长久地保存错误信息。

此外，使用标准错误写入还可以帮助我们调试正在运行的程序，而不必停止并重新编写或调试代码。这在开发过程中非常有用，可以节省大量的时间和精力。

##另请参阅

- [PHP error_log() 函数文档](https://www.php.net/manual/zh/function.error-log.php)
- [PHP 错误处理](https://www.php.net/manual/zh/book.errorfunc.php)
- [PHP 调试技巧](https://www.php.net/manual/zh/debugger.php)