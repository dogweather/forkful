---
date: 2024-01-20 17:33:31.497244-07:00
description: "How to: (\u600E\u4E48\u505A\uFF1F) \u5728PHP\u4E2D\uFF0C`DateTime` \u7C7B\
  \u81EAPHP\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:48.184864-06:00'
model: gpt-4-1106-preview
summary: "(\u600E\u4E48\u505A\uFF1F) \u5728PHP\u4E2D\uFF0C`DateTime` \u7C7B\u81EA\
  PHP 5.2\u8D77\u5C31\u6210\u4E3A\u5904\u7406\u65E5\u671F\u548C\u65F6\u95F4\u7684\u9996\
  \u9009\u3002\u5BF9\u6BD4\u5B83\u4EEC\u4E4B\u524D\uFF0C\u4EBA\u4EEC\u4F1A\u7528\u5B57\
  \u7B26\u4E32\u6216\u8005UNIX\u65F6\u95F4\u6233\uFF0C\u4F46\u8FD9\u6837\u4E0D\u591F\
  \u76F4\u89C2\uFF0C\u4E5F\u6613\u51FA\u9519\u3002\u9664\u4E86`DateTime`\uFF0C\u4F60\
  \u8FD8\u53EF\u4EE5\u4F7F\u7528`DateTimeImmutable`\uFF0C\u5B83\u63D0\u4F9B\u4E86\u4E0D\
  \u53EF\u66F4\u6539\u7684\u65E5\u671F\u5BF9\u8C61\u4FDD\u8BC1\uFF0C\u66F4\u5B89\u5168\
  \u3002\u5B9E\u73B0\u7EC6\u8282\u5305\u62EC\u65F6\u533A\u5904\u7406\u548C\u95F0\u79D2\
  \uFF0C\u8FD9\u4E9B\u5728\u5185\u90E8\u5DF2\u7531PHP\u5904\u7406\u3002"
title: "\u6BD4\u8F83\u4E24\u4E2A\u65E5\u671F"
weight: 27
---

## How to: (怎么做？)
```php
<?php
$date1 = new DateTime("2023-03-15");
$date2 = new DateTime("2023-04-01");

if ($date1 < $date2) {
    echo "日期1早于日期2。";
} elseif ($date1 == $date2) {
    echo "两个日期相同。";
} else {
    echo "日期1晚于日期2。";
}
// 输出: 日期1早于日期2。
?>
```

## Deep Dive (深入探索)
在PHP中，`DateTime` 类自PHP 5.2起就成为处理日期和时间的首选。对比它们之前，人们会用字符串或者UNIX时间戳，但这样不够直观，也易出错。除了`DateTime`，你还可以使用`DateTimeImmutable`，它提供了不可更改的日期对象保证，更安全。实现细节包括时区处理和闰秒，这些在内部已由PHP处理。

## See Also (另请参阅)
- PHP 官方文档关于日期和时间的处理: [php.net/manual/en/book.datetime.php](https://www.php.net/manual/en/book.datetime.php)
- 维基百科上关于格里高利历和时间测量的历史: [en.wikipedia.org/wiki/Gregorian_calendar](https://en.wikipedia.org/wiki/Gregorian_calendar)
- Stack Overflow 上关于日期比较的讨论: [stackoverflow.com/questions/tagged/php+date+comparison](https://stackoverflow.com/questions/tagged/php+date+comparison)
