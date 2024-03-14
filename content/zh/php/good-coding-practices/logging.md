---
date: 2024-01-26 01:07:10.244750-07:00
description: "\u65E5\u5FD7\u8BB0\u5F55\u5B9E\u9645\u4E0A\u7C7B\u4F3C\u4E8E\u4E3A\u4EE3\
  \u7801\u4FDD\u7559\u65E5\u8BB0\uFF1B\u5B83\u662F\u8BB0\u5F55\u4E8B\u4EF6\u3001\u9519\
  \u8BEF\u548C\u5E94\u7528\u7A0B\u5E8F\u8FD0\u884C\u65F6\u53D1\u751F\u7684\u5176\u4ED6\
  \u91CD\u8981\u6570\u636E\u70B9\u7684\u884C\u4E3A\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\
  \u505A\u662F\u4E3A\u4E86\u8DDF\u8E2A\u5E95\u5C42\u53D1\u751F\u7684\u60C5\u51B5\uFF0C\
  \u8C03\u8BD5\u95EE\u9898\uFF0C\u5E76\u4E3A\u4EE5\u540E\u7684\u5206\u6790\u6216\u5408\
  \u89C4\u76EE\u7684\u4FDD\u7559\u5BA1\u8BA1\u8F68\u8FF9\u3002"
lastmod: '2024-03-13T22:44:47.871152-06:00'
model: gpt-4-1106-preview
summary: "\u65E5\u5FD7\u8BB0\u5F55\u5B9E\u9645\u4E0A\u7C7B\u4F3C\u4E8E\u4E3A\u4EE3\
  \u7801\u4FDD\u7559\u65E5\u8BB0\uFF1B\u5B83\u662F\u8BB0\u5F55\u4E8B\u4EF6\u3001\u9519\
  \u8BEF\u548C\u5E94\u7528\u7A0B\u5E8F\u8FD0\u884C\u65F6\u53D1\u751F\u7684\u5176\u4ED6\
  \u91CD\u8981\u6570\u636E\u70B9\u7684\u884C\u4E3A\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\
  \u505A\u662F\u4E3A\u4E86\u8DDF\u8E2A\u5E95\u5C42\u53D1\u751F\u7684\u60C5\u51B5\uFF0C\
  \u8C03\u8BD5\u95EE\u9898\uFF0C\u5E76\u4E3A\u4EE5\u540E\u7684\u5206\u6790\u6216\u5408\
  \u89C4\u76EE\u7684\u4FDD\u7559\u5BA1\u8BA1\u8F68\u8FF9\u3002"
title: "\u65E5\u5FD7\u8BB0\u5F55"
---

{{< edit_this_page >}}

## 什么 & 为什么？

日志记录实际上类似于为代码保留日记；它是记录事件、错误和应用程序运行时发生的其他重要数据点的行为。程序员这样做是为了跟踪底层发生的情况，调试问题，并为以后的分析或合规目的保留审计轨迹。

## 怎样做：

PHP自带了一个易于使用的内置错误日志记录函数。只需要在你的代码中插入`error_log()`，就可以将消息发送到服务器日志。你还可以自定义它，使其写入到特定文件。

```php
<?php
// 记录一个简单的信息消息
error_log("这是一条信息日志条目。");

// 记录一个错误消息
error_log("这是一个错误日志条目。", 0);

// 记录到指定文件
file_put_contents('/path/to/your/custom.log', "一个自定义的日志条目。\n", FILE_APPEND);

// 使用Monolog进行结构化日志记录
require 'vendor/autoload.php';
use Monolog\Logger;
use Monolog\Handler\StreamHandler;

// 创建日志记录器
$logger = new Logger('name');
// 现在添加一些处理程序
$logger->pushHandler(new StreamHandler('/path/to/your/monolog.log', Logger::WARNING));

// 现在你可以使用你的日志记录器了
$logger->warning('这是一个警告日志！');
$logger->error('这是一个错误日志！');
?>
```

这将会以纯文本格式将你的日志输出到服务器日志或你指定的文件中。

## 深入了解：

历史上，PHP开发人员依赖`error_log()`函数或Apache/Nginx日志来捕获问题，但是当需要解析纯文本文件并且没有简单的筛选或排序方式时，这可能会变得混乱。Monolog等日志库的出现开启了PHP中结构化日志记录的时代。这些解决方案通过提供多个日志通道、严重性级别和格式化输出（如JSON，这对于编程化解析来说是梦寐以求的）给你更好的控制。

Monolog的替代品包括Log4php、KLogger和Apache的Log4php。实施方面，强大的日志记录不仅仅是到处倾倒数据，还要考虑诸如日志轮换、存档策略以及与监控工具的集成，以便真正有用。

你应该记住[PSR-3日志记录器接口](https://www.php-fig.org/psr/psr-3/)，它概述了日志库的通用接口，确保了互操作性和一致的访问日志机制方式。

## 另请参见：

- [Monolog GitHub 仓库](https://github.com/Seldaek/monolog)
- [PSR-3日志记录器接口规范](https://www.php-fig.org/psr/psr-3/)
- [PHP错误日志文档](https://www.php.net/manual/en/function.error-log.php)
- [KLogger：一个简单的PHP日志记录类](https://github.com/katzgrau/KLogger)
- [Log4php：一个多功能的PHP日志记录框架](https://logging.apache.org/log4php/)

从内置函数开始尝试，但为了更可维护和可扩展的方法，考虑投入时间熟悉像Monolog这样的库。日志记录愉快！
