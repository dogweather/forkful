---
date: 2024-01-26 01:07:10.244750-07:00
description: "\u600E\u6837\u505A\uFF1A PHP\u81EA\u5E26\u4E86\u4E00\u4E2A\u6613\u4E8E\
  \u4F7F\u7528\u7684\u5185\u7F6E\u9519\u8BEF\u65E5\u5FD7\u8BB0\u5F55\u51FD\u6570\u3002\
  \u53EA\u9700\u8981\u5728\u4F60\u7684\u4EE3\u7801\u4E2D\u63D2\u5165`error_log()`\uFF0C\
  \u5C31\u53EF\u4EE5\u5C06\u6D88\u606F\u53D1\u9001\u5230\u670D\u52A1\u5668\u65E5\u5FD7\
  \u3002\u4F60\u8FD8\u53EF\u4EE5\u81EA\u5B9A\u4E49\u5B83\uFF0C\u4F7F\u5176\u5199\u5165\
  \u5230\u7279\u5B9A\u6587\u4EF6\u3002"
lastmod: '2024-04-05T21:53:48.178806-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u65E5\u5FD7\u8BB0\u5F55"
weight: 17
---

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
