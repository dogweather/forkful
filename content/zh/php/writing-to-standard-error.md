---
title:                "写入标准错误"
html_title:           "Arduino: 写入标准错误"
simple_title:         "写入标准错误"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)

在PHP中，写入标准错误（STDERR）允许你把错误、警告和日志分开于正常输出。程序员这么做可以更好地控制和调试程序。

## How to: (如何执行：)

使用 fopen() 打开 STDERR 流，然后用 fwrite() 写入信息。示例如下：

```PHP
<?php
$stderr = fopen('php://stderr', 'w');
fwrite($stderr, "发生了一个错误。\n");
fclose($stderr);
?>
```

样本输出（在控制台）：

```
发生了一个错误。
```

或者直接使用预定义的常量 STDERR：

```PHP
<?php
fwrite(STDERR, "发生了一个错误。\n");
?>
```

## Deep Dive (深入了解)

历史上，标准错误是 UNIX 系统中的概念，它用于独立记录错误信息。PHP作为跨平台语言，沿用了这一概念。备选方案包括将错误写入文件或其他日志系统。实现细节上，PHP内建有多个错误处理函数和异常处理机制，但直接写入 STDERR 是最底层直接的方法。

## See Also (另请参阅)

- [PHP手册：错误处理和日志](https://www.php.net/manual/zh/book.errorfunc.php)
- [PHP 手册：预定义变量](https://www.php.net/manual/zh/reserved.variables.php)
- [UNIX标准输出和错误解释](https://en.wikipedia.org/wiki/Standard_streams#Standard_error_(stderr))
