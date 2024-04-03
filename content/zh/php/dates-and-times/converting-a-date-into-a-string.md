---
date: 2024-01-20 17:37:13.306916-07:00
description: "\u5728PHP\u4E2D\uFF0C\u65E5\u671F\u5230\u5B57\u7B26\u4E32\u7684\u8F6C\
  \u6362\u662F\u5C06\u65E5\u671F\u683C\u5F0F\u5316\u4E3A\u53EF\u8BFB\u5F62\u5F0F\u7684\
  \u8FC7\u7A0B\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u5C55\u793A\
  \u5BF9\u7528\u6237\u53CB\u597D\u7684\u65E5\u671F\u6216\u8005\u5C06\u65E5\u671F\u5B58\
  \u50A8\u5230\u6570\u636E\u5E93\u4E2D\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.876790-06:00'
model: gpt-4-1106-preview
summary: "\u5728PHP\u4E2D\uFF0C\u65E5\u671F\u5230\u5B57\u7B26\u4E32\u7684\u8F6C\u6362\
  \u662F\u5C06\u65E5\u671F\u683C\u5F0F\u5316\u4E3A\u53EF\u8BFB\u5F62\u5F0F\u7684\u8FC7\
  \u7A0B\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u5C55\u793A\u5BF9\
  \u7528\u6237\u53CB\u597D\u7684\u65E5\u671F\u6216\u8005\u5C06\u65E5\u671F\u5B58\u50A8\
  \u5230\u6570\u636E\u5E93\u4E2D\u3002."
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
