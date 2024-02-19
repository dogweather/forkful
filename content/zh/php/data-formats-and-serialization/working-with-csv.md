---
aliases:
- /zh/php/working-with-csv/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:06.425176-07:00
description: "\u7528 CSV\uFF08\u9017\u53F7\u5206\u9694\u503C\uFF09\u5DE5\u4F5C\u6D89\
  \u53CA\u4ECE CSV \u6587\u4EF6\u8BFB\u53D6\u548C\u5411 CSV \u6587\u4EF6\u5199\u5165\
  \u6570\u636E\uFF0C\u8FD9\u662F\u4E00\u79CD\u8868\u793A\u5E73\u9762\u6587\u672C\u4E2D\
  \u7684\u8868\u683C\u6570\u636E\u7684\u6D41\u884C\u683C\u5F0F\u3002\u7A0B\u5E8F\u5458\
  \u4E4B\u6240\u4EE5\u4F7F\u7528\u5B83\uFF0C\u662F\u56E0\u4E3A\u5B83\u7684\u7B80\u5355\
  \u6027\u548C\u5728\u8DE8\u5E73\u53F0\u53CA\u7F16\u7A0B\u8BED\u8A00\u4E2D\u7684\u5E7F\
  \u6CDB\u652F\u6301\uFF0C\u4F7F\u5F97\u6570\u636E\u5728\u4E0D\u540C\u7A0B\u5E8F\u3001\
  \u7CFB\u7EDF\u6216\u6570\u636E\u5E93\u4E4B\u95F4\u7684\u4EA4\u6362\u53D8\u5F97\u5BB9\
  \u6613\u3002"
lastmod: 2024-02-18 23:08:59.236278
model: gpt-4-0125-preview
summary: "\u7528 CSV\uFF08\u9017\u53F7\u5206\u9694\u503C\uFF09\u5DE5\u4F5C\u6D89\u53CA\
  \u4ECE CSV \u6587\u4EF6\u8BFB\u53D6\u548C\u5411 CSV \u6587\u4EF6\u5199\u5165\u6570\
  \u636E\uFF0C\u8FD9\u662F\u4E00\u79CD\u8868\u793A\u5E73\u9762\u6587\u672C\u4E2D\u7684\
  \u8868\u683C\u6570\u636E\u7684\u6D41\u884C\u683C\u5F0F\u3002\u7A0B\u5E8F\u5458\u4E4B\
  \u6240\u4EE5\u4F7F\u7528\u5B83\uFF0C\u662F\u56E0\u4E3A\u5B83\u7684\u7B80\u5355\u6027\
  \u548C\u5728\u8DE8\u5E73\u53F0\u53CA\u7F16\u7A0B\u8BED\u8A00\u4E2D\u7684\u5E7F\u6CDB\
  \u652F\u6301\uFF0C\u4F7F\u5F97\u6570\u636E\u5728\u4E0D\u540C\u7A0B\u5E8F\u3001\u7CFB\
  \u7EDF\u6216\u6570\u636E\u5E93\u4E4B\u95F4\u7684\u4EA4\u6362\u53D8\u5F97\u5BB9\u6613\
  \u3002"
title: "\u5904\u7406CSV\u6587\u4EF6"
---

{{< edit_this_page >}}

## 什么 & 为什么？

用 CSV（逗号分隔值）工作涉及从 CSV 文件读取和向 CSV 文件写入数据，这是一种表示平面文本中的表格数据的流行格式。程序员之所以使用它，是因为它的简单性和在跨平台及编程语言中的广泛支持，使得数据在不同程序、系统或数据库之间的交换变得容易。

## 如何操作：

PHP 提供了内置函数处理 CSV 文件，使得读取和写入这些文件变得简单，无需第三方库。以下是一些入门示例：

### 读取 CSV 文件

你可以使用 `fopen()` 结合 `fgetcsv()` 打开 CSV 文件并读取其内容：

```php
<?php
$filename = 'data.csv';
$handle = fopen($filename, "r");
if ($handle !== FALSE) {
    while (($data = fgetcsv($handle, 1000, ",")) !== FALSE) {
        $num = count($data);
        echo "行中字段数量: $num\n";
        for ($c = 0; $c < $num; $c++) {
            echo $data[$c] . "\n";
        }
    }
    fclose($handle);
}
?>
```

该脚本打印出每行的字段数量，接着打印每个字段的内容。

### 写入 CSV 文件

要写入 CSV 文件，使用 `fopen()` 的写模式（`w`）和 `fputcsv()`：

```php
<?php
$list = [
    ['ID', '姓名', '电子邮件'],
    [1, 'John Doe', 'john@example.com'],
    [2, 'Jane Doe', 'jane@example.com']
];

$handle = fopen('users.csv', 'w');

foreach ($list as $row) {
    fputcsv($handle, $row);
}

fclose($handle);
?>
```

该脚本创建一个名为 `users.csv` 的文件，并向其写入标题和两行数据。

### 使用库：League\Csv

对于更高级的 CSV 处理，`League\Csv` 库提供了一套强大的功能。通过 Composer 安装它（`composer require league/csv`）后，你可以更灵活地读写 CSV 数据。

#### 使用 League\Csv 读取

```php
<?php
require 'vendor/autoload.php';

use League\Csv\Reader;

$csv = Reader::createFromPath('data.csv', 'r');
$csv->setHeaderOffset(0); // 如果你想使用第一行作为表头，请设置这个

$results = $csv->getRecords();
foreach ($results as $row) {
    print_r($row);
}
?>
```

该脚本读取 `data.csv`，将第一行视为列标题，并将每行打印为关联数组。

#### 使用 League\Csv 写入

```php
<?php
require 'vendor/autoload.php';

use League\Csv\Writer;

$csv = Writer::createFromPath('users_new.csv', 'w+');

$csv->insertOne(['ID', '姓名', '电子邮件']);
$csv->insertAll([
    [3, 'Alex Doe', 'alex@example.com'],
    [4, 'Anna Smith', 'anna@example.com']
]);

echo "成功写入 users_new.csv。";
?>
```

这将创建 `users_new.csv` 并写入一个标题行，随后是两行数据。
