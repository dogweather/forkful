---
title:                "csv操作指南"
html_title:           "PHP: csv操作指南"
simple_title:         "csv操作指南"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/working-with-csv.md"
---

{{< edit_this_page >}}

# 为什么使用CSV

CSV是一种常见的数据交换格式，它允许我们使用逗号分隔的值来存储和传输数据。在PHP中，CSV文件经常被用来处理大量的数据，比如电子表格或数据库导出的数据。因此，学习如何使用CSV文件可以帮助我们更有效地处理数据。

## 如何操作CSV文件

让我们来看一下如何使用PHP代码来读取和处理CSV文件。首先，我们需要使用PHP的内置函数`fopen()`来打开CSV文件：

```PHP
$handle = fopen("users.csv", "r");
```

这将返回一个指向CSV文件的资源指针，我们可以使用`fgetcsv()`函数逐行读取CSV文件的内容：

```PHP
while (($data = fgetcsv($handle)) !== false) {
  // 在这里处理CSV数据
}
```

每次循环，`$data`都会被赋值为当前行的值的数组。例如，如果我们的CSV文件有三列姓名、年龄和性别，那么`$data`将包含这三个值。我们可以根据需要使用这些值来执行任何操作。

此外，我们也可以使用`fputcsv()`函数来将数据写入CSV文件。例如，如果我们有一个包含学生信息的关联数组，我们可以将它写入CSV文件中：

```PHP
$students = [
  ["name" => "张三", "age" => "18", "gender" => "男"],
  ["name" => "李四", "age" => "19", "gender" => "女"],
  ["name" => "王五", "age" => "20", "gender" => "男"]
];

$handle = fopen("students.csv", "w");

foreach ($students as $student) {
  fputcsv($handle, $student);
}
```

这将会在CSV文件中创建三行数据，每一行都包含学生的姓名、年龄和性别。

## 深入了解CSV文件

CSV文件在处理大量数据时非常有用，特别是在与数据库交互时。PHP提供了多种函数来处理CSV文件，例如：

- `fgetcsv()`：用于读取CSV文件中的行数据，并将其存储在数组中。
- `fputcsv()`：用于将数据写入CSV文件，它接受一个数组作为参数，并将其转换成逗号分隔的值。
- `fgetcsv()`：用于关闭CSV文件的资源指针。

我们也可以使用第三方库来提高处理CSV文件的效率，例如`league/csv`。此外，还有一些PHP框架提供了方便的CSV操作功能，比如Laravel的Eloquent CSV包。

除了读取和写入数据，我们也可以使用PHP的其他功能来处理CSV文件。例如，我们可以使用`array_map()`函数来对CSV数据进行转换，或者使用`array_filter()`函数来过滤数据。

# 查看更多

- PHP官方文档：http://php.net/manual/en/function.fgetcsv.php
- `league/csv`库：https://csv.thephpleague.com/
- Laravel Eloquent CSV包：https://laravel.com/docs/5.8/eloquent#csv-exports