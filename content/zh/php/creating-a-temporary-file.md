---
title:                "创建临时文件"
html_title:           "PHP: 创建临时文件"
simple_title:         "创建临时文件"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？

创建临时文件是指在程序运行过程中临时保存数据的操作。程序员会这样做是为了在需要的时候能够方便地访问和处理这些数据。

## 如何：

```PHP
// 创建一个临时文件
$tmp_file = tmpfile();

// 将数据写入临时文件
fwrite($tmp_file, "Hello world");

// 读取临时文件中的数据
rewind($tmp_file);
echo fgets($tmp_file); // 输出：Hello world

// 关闭临时文件
fclose($tmp_file);
```

## 深入了解：

1. 创建临时文件的概念最初起源于操作系统的需要，在早期的计算机系统中，内存和存储空间非常有限，所以程序需要在运行时创建临时文件来保存数据。
2. 除了使用临时文件，还有一些其他的方法可以临时存储数据，如使用内存缓冲区或者数据库。
3. 在PHP中，我们可以使用`tmpfile()`函数来创建一个临时文件，它会返回一个文件指针，我们可以使用标准的文件操作函数来读写数据。

## 参考资料：

- [PHP 官方文档 - `tmpfile()`](https://www.php.net/manual/zh/function.tmpfile.php)
- [How to Create a Temporary File in PHP](https://www.geeksforgeeks.org/how-to-create-a-temporary-file-in-php/)