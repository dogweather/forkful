---
date: 2024-01-20 17:37:13.306916-07:00
description: "\u5982\u4F55\u505A\uFF1A ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.876790-06:00'
model: gpt-4-1106-preview
summary: .
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
