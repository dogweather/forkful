---
title:                "创建临时文件"
html_title:           "Kotlin: 创建临时文件"
simple_title:         "创建临时文件"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 什么是临时文件，以及为什么需要它？

临时文件是程序执行过程中短暂产生的文件。程序员创建临时文件是为了保存临时数据，存储缓冲区，或辅助文件转换等。

## 如何创建：

下面是一个使用PHP创建临时文件的样例代码。使用`tempnam()`函数生成一个独一无二的文件名，然后使用 `fopen()`函数创建这个文件。

```PHP
$temp_file = tempnam(sys_get_temp_dir(), 'Tux');

$handle = fopen($temp_file, "w");
fwrite($handle, "Hello, Tux!");
fclose($handle);

echo "Temporary file name: $temp_file";

// Read from the temporary file
$handle = fopen($temp_file, "r");
$content = fread($handle, filesize($temp_file));
fclose($handle);
echo "Content: $content";
```

这个代码先创建了一个临时文件，写入了"Hello, Tux!"，然后再读出了文件内容。

## 深度理解：

在早期的操作系统或者编程语言中，并没有直接提供创建临时文件的接口，程序员需要手动创建并确保文件名的唯一性。而现在的PHP给出了`tempnam()`函数来帮助我们方便地创建临时文件。但这并不是唯一方法，您还可以使用`tmpfile()`函数直接返回一个文件句柄，这个函数会在文件关闭的时候自动删除临时文件。这个文件会被存储在系统的临时目录下，可以通过`sys_get_temp_dir()`函数获取这个目录。

## 延伸阅读：

PHP文档介绍了一些和临时文件相关的函数，如`tmpfile()`、`tempnam()`和`sys_get_temp_dir()`
- `tmpfile()` https://www.php.net/manual/en/function.tmpfile.php
- `tempnam()` https://www.php.net/manual/en/function.tempnam.php
- `sys_get_temp_dir()` https://www.php.net/manual/en/function.sys-get-temp-dir.php

其他相关的知识：
- 文件的读写 https://www.w3schools.com/php/php_file_open.asp
- 关于临时文件的安全操作 https://secure.php.net/manual/en/features.file-upload.common-pitfalls.php