---
title:                "将日期转换为字符串"
date:                  2024-01-20T17:37:13.306916-07:00
model:                 gpt-4-1106-preview
simple_title:         "将日期转换为字符串"

category:             "PHP"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
在PHP中，日期到字符串的转换是将日期格式化为可读形式的过程。程序员这么做是为了展示对用户友好的日期或者将日期存储到数据库中。

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
