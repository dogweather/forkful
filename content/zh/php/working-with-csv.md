---
title:                "处理 CSV 文件"
date:                  2024-01-19
html_title:           "Bash: 处理 CSV 文件"
simple_title:         "处理 CSV 文件"

category:             "PHP"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? 什么是 CSV 及为什么使用?

CSV (逗号分隔值) 是一种简单的文件格式，用于储存表格数据。程序员使用它因为它简单、兼容性好，方便数据交换。

## How to: 实践篇

读取 CSV 文件:

```php
<?php
$filename = 'data.csv';
if (($handle = fopen($filename, 'r')) !== false) {
    while (($data = fgetcsv($handle, 1000, ",")) !== false) {
        print_r($data);
    }
    fclose($handle);
}
?>
```

写入 CSV 文件:

```php
<?php
$list = array (
    array('编号', '姓名', '年龄'),
    array('1', '张三', '25'),
    array('2', '李四', '27')
);

$fp = fopen('file.csv', 'w');

foreach ($list as $fields) {
    fputcsv($fp, $fields);
}

fclose($fp);
?>
```

## Deep Dive 深入探讨

CSV 格式源自早期办公软件，并广泛用于数据交换。它是 XML 和 JSON 格式的替代品之一，尤其适合简单的表格数据。值得注意的是它无法很好地处理多层嵌套或数据类型。PHP 提供了内建函数来处理 CSV 文件，如 `fgetcsv` 和 `fputcsv`，它们让读写操作变得简单。

## See Also 参考链接

- CSV 格式规范: [tools.ietf.org/html/rfc4180](https://tools.ietf.org/html/rfc4180)
- 一个关于 PHP 和 CSV 交互处理的教程: [phpenthusiast.com/blog/parse-csv-with-php](https://phpenthusiast.com/blog/parse-csv-with-php)
