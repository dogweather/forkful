---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:58.488797-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728 PHP \u4E2D\uFF0C\u53EF\u4EE5\u4F7F\
  \u7528 `fwrite()` \u51FD\u6570\u4EE5\u53CA\u9884\u5B9A\u4E49\u5E38\u91CF `STDERR`\uFF08\
  \u4EE3\u8868\u9519\u8BEF\u8F93\u51FA\u6D41\uFF09\u6765\u5B9E\u73B0\u5199\u5165 stderr\u3002"
lastmod: '2024-03-13T22:44:47.882506-06:00'
model: gpt-4-0125-preview
summary: "\u5728 PHP \u4E2D\uFF0C\u53EF\u4EE5\u4F7F\u7528 `fwrite()` \u51FD\u6570\u4EE5\
  \u53CA\u9884\u5B9A\u4E49\u5E38\u91CF `STDERR`\uFF08\u4EE3\u8868\u9519\u8BEF\u8F93\
  \u51FA\u6D41\uFF09\u6765\u5B9E\u73B0\u5199\u5165 stderr."
title: "\u5199\u5165\u6807\u51C6\u9519\u8BEF"
weight: 25
---

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
