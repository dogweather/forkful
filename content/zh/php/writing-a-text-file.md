---
title:                "编写文本文件"
html_title:           "PHP: 编写文本文件"
simple_title:         "编写文本文件"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 什么是文本文件 & 为什么要写?

写入文本文件是将数据写入到计算机中的某一个文件中。程序员经常写入文本文件，因为它是一种非常有效的存储和处理数据的方式。通过将数据保存在文本文件中，程序员可以方便地读取和修改数据，从而使他们的程序更加灵活和可靠。

## 如何实现:

```PHP
$file = fopen("data.txt", "w"); // 创建一个名为data.txt的文本文件，模式为"w"表示写入模式
fwrite($file, "Hello World!"); // 将文本数据 "Hello World!" 写入到文件中
fclose($file); // 关闭文件资源
```

以下为打印出文本文件中的内容：

```PHP
$file = fopen("data.txt", "r"); // 打开data.txt文件，模式为"r"表示读取模式
$contents = fread($file, filesize("data.txt")); // 读取文件内容并存储在变量 $contents 中
fclose($file); // 关闭文件资源
echo $contents; // 打印出文件内容，即输出 "Hello World!"
```

## 深入了解:

- 文本文件是一种简单的文件格式，它主要由文本数据组成。
- 除了写入和读取文本文件外，程序员也可以使用数据库或其他数据存储方式来处理数据。
- 在写入文本文件时，程序员可以选择不同的写入模式，如"w"、"a"等，具体模式的含义可以参考官方文档或其他相关资源。

## 参考资料:

- [PHP 官方文档](https://www.php.net/manual/en/function.fwrite.php)
- [PHP 程序员指南](https://www.php.net/manual/en/refs.basic.other.php.php)