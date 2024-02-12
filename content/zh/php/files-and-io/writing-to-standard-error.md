---
title:                "写入标准错误"
aliases:
- /zh/php/writing-to-standard-error.md
date:                  2024-02-03T19:33:58.488797-07:00
model:                 gpt-4-0125-preview
simple_title:         "写入标准错误"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么与为什么？

在 PHP 中写入标准错误（stderr）涉及到将错误信息或诊断信息与标准输出（stdout）分开，使开发人员能够更好地管理他们的输出流，以便于调试和记录。程序员利用这一技术确保错误信息不会干扰程序的输出，使监控和故障排查应用程序变得更加容易。

## 如何操作：

在 PHP 中，可以使用 `fwrite()` 函数以及预定义常量 `STDERR`（代表错误输出流）来实现写入 stderr。

```php
<?php
// 向 stderr 写入一个简单的信息。
fwrite(STDERR, "这是一个错误消息。\n");
```

当从命令行执行脚本时的示例输出：
```
这是一个错误消息。
```

为了展示更实际的用法，考虑一个你正在解析用户输入并遇到意外数据的场景：
```php
<?php
$input = 'unexpected data';

// 模拟处理用户输入的错误。
if ($input === 'unexpected data') {
    fwrite(STDERR, "错误：收到了意外的输入。\n");
    exit(1); // 以非零值退出，表明有错误。
}
```

虽然 PHP 的内置 stderr 处理能力通常已经足够，但是在处理更复杂的应用程序或希望将 stderr 日志记录与外部系统集成时，第三方库如 Monolog 可以是强大的帮手。Monolog 是一个日志记录库，它可以处理 stderr 以及许多其他目标（文件、套接字等）。

使用 Monolog 写入 stderr：

首先，确保通过 Composer 安装了 Monolog：
```
composer require monolog/monolog
```

然后，您可以配置 Monolog 使用针对 `php://stderr` 的 `StreamHandler`：

```php
<?php
require 'vendor/autoload.php';

use Monolog\Logger;
use Monolog\Handler\StreamHandler;

// 创建日志频道
$log = new Logger('name');
$log->pushHandler(new StreamHandler('php://stderr', Logger::WARNING));

// 向 stderr 添加一条日志消息
$log->warning('这是一个警告消息。');
```

上述代码利用 Monolog 向 stderr 发送警告消息，这对于需要详细日志配置或外部日志监控的应用程序非常有用。
