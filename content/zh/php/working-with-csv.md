---
title:                "处理CSV文件"
html_title:           "PHP: 处理CSV文件"
simple_title:         "处理CSV文件"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/working-with-csv.md"
---

{{< edit_this_page >}}

## 什么是CSV？ 为什么要使用它？

CSV（逗号分隔值）是一种用于存储数据的文件格式，它使用逗号来分隔不同的数据字段。程序员通常会使用CSV来导入和导出数据，以便在不同的应用程序之间共享。它是一种简单且易于修改和读取的文件格式，因此在数据处理中非常有用。

## 如何操作CSV

### 读取CSV文件
要读取一个CSV文件，我们可以使用`fgetcsv()`函数来将其读取为一个数组。假设我们有一个名为`data.csv`的文件，它包含以下数据：
```
id,name,age
1,John,30
2,Jane,25
3,Bob,35
```
我们可以使用以下代码来读取该文件：
```
<?php
$file = fopen('data.csv', 'r'); // 打开文件
$data = fgetcsv($file); // 将文件读取为数组
fclose($file); // 关闭文件
var_dump($data); // 输出数组内容
```
此代码将输出以下结果：
```
array(3) {
   [0] => string(2) "id"
   [1] => string(4) "name"
   [2] => string(3) "age"
}
```

### 写入CSV文件
要将数据写入CSV文件，我们可以使用`fputcsv()`函数。假设我们要在`data.csv`文件的末尾添加一行数据`4,Sarah,27`，我们可以使用以下代码：
```
<?php
$file = fopen('data.csv', 'a'); // 打开文件，以追加模式写入
$data = ['4', 'Sarah', '27']; // 要写入的数据数组
fputcsv($file, $data); // 将数据写入文件
fclose($file); // 关闭文件
```

## 深入了解CSV

### 历史背景
CSV文件格式最初是在20世纪70年代由IBM开发的，它被用于存储大量数据库数据。随着互联网和电子表格软件的发展，CSV也成为了一种常见的数据交换格式。

### 其他选择
除了CSV，还有许多其他数据交换格式，如JSON、XML和YAML。每种格式都有不同的优势和用途，程序员需要根据实际情况选择最合适的格式。

### 实现细节
在PHP中，CSV相关的函数大多都在`fgetcsv()`和`fputcsv()`两个函数中。它们提供了一种简单的接口来读取和写入CSV文件，同时也有一些可选的参数来定制数据分隔符、引号符和从文件读取的最大字符数等。

## 查看更多资料

- [PHP官方文档 - CSV函数](https://www.php.net/manual/en/ref.csv.php)
- [W3Schools - PHP CSV教程](https://www.w3schools.com/php/php_csv.asp)
- [IBM Developer - CSV文件格式简介](https://developer.ibm.com/articles/l-php-csv/)