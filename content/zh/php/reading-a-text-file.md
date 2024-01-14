---
title:                "PHP: 读取文本文件"
simple_title:         "读取文本文件"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

为什么：阅读文本文件的重要性对于学习PHP编程来说十分重要。通过阅读文本文件，您可以学习如何处理不同类型的数据，以及如何编写更有效的代码。

如何：首先，我们需要使用PHP的`fopen()`函数来打开一个文本文件。然后，使用`fgets()`函数按行读取文件内容，并使用循环来输出每一行的内容。最后，使用`fclose()`函数关闭文件。

```PHP
<?php
$file = fopen("text.txt", "r"); // 打开文本文件
while(!feof($file)){ // 循环直到文件结尾
    $line = fgets($file); // 读取一行内容
    echo $line; // 输出该行内容
}
fclose($file); // 关闭文件
?>
```

输出：

```
这是第一行文本
这是第二行文本
这是第三行文本
```

深入：除了使用`fgets()`函数，还可以使用`file()`函数来一次性读取整个文件的内容，并保存在一个数组中。此外，您还可以使用`fgetc()`函数来按字符读取文件内容。

另外，还有许多其他的文件读取函数可以在PHP手册中找到。通过深入学习这些函数，您可以更加灵活地处理文本文件中的数据，并且能够应用到实际的PHP编程项目中。

您也可以在文本文件中存储特定格式的数据，例如CSV格式的表格数据。通过使用PHP的文件读取函数，您可以轻松地将这些数据导入到您的程序中，从而节省大量重复的手动输入工作。

见也：

- [PHP文件读取教程](https://www.php.net/manual/zh/function.fgets.php)
- [CSV文件格式介绍](https://www.guru99.com/csv-file.html)
- [PHP手册](https://www.php.net/manual/zh/index.php)