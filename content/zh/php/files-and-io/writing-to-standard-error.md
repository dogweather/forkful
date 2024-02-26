---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:58.488797-07:00
description: "\u5728 PHP \u4E2D\u5199\u5165\u6807\u51C6\u9519\u8BEF\uFF08stderr\uFF09\
  \u6D89\u53CA\u5230\u5C06\u9519\u8BEF\u4FE1\u606F\u6216\u8BCA\u65AD\u4FE1\u606F\u4E0E\
  \u6807\u51C6\u8F93\u51FA\uFF08stdout\uFF09\u5206\u5F00\uFF0C\u4F7F\u5F00\u53D1\u4EBA\
  \u5458\u80FD\u591F\u66F4\u597D\u5730\u7BA1\u7406\u4ED6\u4EEC\u7684\u8F93\u51FA\u6D41\
  \uFF0C\u4EE5\u4FBF\u4E8E\u8C03\u8BD5\u548C\u8BB0\u5F55\u3002\u7A0B\u5E8F\u5458\u5229\
  \u7528\u8FD9\u4E00\u6280\u672F\u786E\u4FDD\u9519\u8BEF\u4FE1\u606F\u4E0D\u4F1A\u5E72\
  \u6270\u7A0B\u5E8F\u7684\u8F93\u51FA\uFF0C\u4F7F\u76D1\u63A7\u548C\u6545\u969C\u6392\
  \u67E5\u5E94\u7528\u7A0B\u5E8F\u53D8\u5F97\u66F4\u52A0\u5BB9\u6613\u3002"
lastmod: '2024-02-25T18:49:45.450223-07:00'
model: gpt-4-0125-preview
summary: "\u5728 PHP \u4E2D\u5199\u5165\u6807\u51C6\u9519\u8BEF\uFF08stderr\uFF09\u6D89\
  \u53CA\u5230\u5C06\u9519\u8BEF\u4FE1\u606F\u6216\u8BCA\u65AD\u4FE1\u606F\u4E0E\u6807\
  \u51C6\u8F93\u51FA\uFF08stdout\uFF09\u5206\u5F00\uFF0C\u4F7F\u5F00\u53D1\u4EBA\u5458\
  \u80FD\u591F\u66F4\u597D\u5730\u7BA1\u7406\u4ED6\u4EEC\u7684\u8F93\u51FA\u6D41\uFF0C\
  \u4EE5\u4FBF\u4E8E\u8C03\u8BD5\u548C\u8BB0\u5F55\u3002\u7A0B\u5E8F\u5458\u5229\u7528\
  \u8FD9\u4E00\u6280\u672F\u786E\u4FDD\u9519\u8BEF\u4FE1\u606F\u4E0D\u4F1A\u5E72\u6270\
  \u7A0B\u5E8F\u7684\u8F93\u51FA\uFF0C\u4F7F\u76D1\u63A7\u548C\u6545\u969C\u6392\u67E5\
  \u5E94\u7528\u7A0B\u5E8F\u53D8\u5F97\u66F4\u52A0\u5BB9\u6613\u3002"
title: "\u5199\u5165\u6807\u51C6\u9519\u8BEF"
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
