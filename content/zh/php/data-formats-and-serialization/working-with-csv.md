---
title:                "处理CSV文件"
aliases:
- /zh/php/working-with-csv.md
date:                  2024-02-03T19:21:06.425176-07:00
model:                 gpt-4-0125-preview
simple_title:         "处理CSV文件"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
