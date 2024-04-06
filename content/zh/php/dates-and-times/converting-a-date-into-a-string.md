---
date: 2024-01-20 17:37:13.306916-07:00
description: "\u5982\u4F55\u505A\uFF1A \u65E5\u671F\u5230\u5B57\u7B26\u4E32\u7684\u8F6C\
  \u6362\u5728PHP\u4E2D\u5386\u53F2\u60A0\u4E45\u3002\u521D\u671F\u7684PHP\u7248\u672C\
  \u4F7F\u7528`date()`\u51FD\u6570\u8FDB\u884C\u7B80\u5355\u8F6C\u6362\u3002\u53D1\
  \u5C55\u5230\u73B0\u5728\uFF0C`DateTime`\u7C7B\u63D0\u4F9B\u4E86\u66F4\u591A\u9009\
  \u9879\u548C\u65F6\u533A\u652F\u6301\u3002\u66FF\u4EE3\u65B9\u6848\u5305\u62EC\u4F7F\
  \u7528`IntlDateFormatter`\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:38:47.034199-06:00'
model: gpt-4-1106-preview
summary: "\u5982\u4F55\u505A\uFF1A \u65E5\u671F\u5230\u5B57\u7B26\u4E32\u7684\u8F6C\
  \u6362\u5728PHP\u4E2D\u5386\u53F2\u60A0\u4E45\u3002\u521D\u671F\u7684PHP\u7248\u672C\
  \u4F7F\u7528`date()`\u51FD\u6570\u8FDB\u884C\u7B80\u5355\u8F6C\u6362\u3002\u53D1\
  \u5C55\u5230\u73B0\u5728\uFF0C`DateTime`\u7C7B\u63D0\u4F9B\u4E86\u66F4\u591A\u9009\
  \u9879\u548C\u65F6\u533A\u652F\u6301\u3002\u66FF\u4EE3\u65B9\u6848\u5305\u62EC\u4F7F\
  \u7528`IntlDateFormatter` \u6216\u8005\u5176\u4ED6\u7B2C\u4E09\u65B9\u5E93\u6765\
  \u5904\u7406\u590D\u6742\u7684\u65E5\u671F\u683C\u5F0F\u5316\u9700\u6C42\u3002\u5728\
  \u5B9E\u9645\u4E2D\uFF0C\u6839\u636E\u9700\u8981\u9009\u62E9\u5408\u9002\u7684\u65B9\
  \u6CD5\u5F88\u91CD\u8981\uFF0C\u4E5F\u5F71\u54CD\u5E94\u7528\u7A0B\u5E8F\u7684\u56FD\
  \u9645\u5316\u548C\u672C\u5730\u5316\u3002"
title: "\u5C06\u65E5\u671F\u8F6C\u6362\u4E3A\u5B57\u7B26\u4E32"
weight: 28
---

## 如何做：
```PHP
<?php
$date = new DateTime('now', new DateTimeZone('Asia/Shanghai'));
echo $date->format('Y-m-d H:i:s'); // 输出样例: 2023-04-05 20:21:00
?>
```

```PHP
<?php
echo date('Y-m-d', strtotime('2023-05-01')); // 输出样例: 2023-05-01
?>
```

## 深入了解
日期到字符串的转换在PHP中历史悠久。初期的PHP版本使用`date()`函数进行简单转换。发展到现在，`DateTime`类提供了更多选项和时区支持。替代方案包括使用`IntlDateFormatter` 或者其他第三方库来处理复杂的日期格式化需求。在实际中，根据需要选择合适的方法很重要，也影响应用程序的国际化和本地化。

## 参考链接
- [PHP官方文档 - date()](https://www.php.net/manual/en/function.date.php)
- [PHP官方文档 - DateTime](https://www.php.net/manual/en/class.datetime.php)
- [PHP官方文档 - IntlDateFormatter](https://www.php.net/manual/en/class.intldateformatter.php)
