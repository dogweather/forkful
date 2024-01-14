---
title:                "PHP: 创建临时文件"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

##为什么？
 创建临时文件对于PHP程序员来说是一个常见的操作。它可以用来存储临时数据，如用户上传的文件，在处理过程中需要暂时存储的数据等。临时文件还可以用来在程序中存储一些中间结果，以便将来可以继续使用。总而言之，创建临时文件是为了更有效地处理数据。

##如何做到？
要创建一个临时文件，首先需要使用PHP内置函数`tempnam()`来生成一个唯一的临时文件名，并指定一个目录进行存储。接下来，可以使用`fopen()`函数以写入模式打开该文件，这样就可以像操作通常的文件一样进行写入。下面是一个简单的示例代码：

```PHP
<?php
// 创建临时文件
$tempFile = tempnam('/tmp', 'temp_');

// 打开文件进行写入
$handle = fopen($tempFile, "w");
fwrite($handle, "这是临时数据");
fclose($handle);

// 读取此临时文件并打印内容
echo file_get_contents($tempFile);
// 输出：这是临时数据

// 删除临时文件
unlink($tempFile);
?>
```

##深入探讨
当使用`tempnam()`函数创建临时文件时，可以指定一个前缀来标识该文件。这一点在处理多个临时文件时很有用。另外，使用`tempnam()`返回的临时文件名称是绝对路径，因此我们不必担心文件名称冲突的问题。一旦不再需要使用临时文件，我们可以使用`unlink()`函数将其删除，以节省磁盘空间。

另外，PHP还提供了`tmpfile()`函数来直接创建临时文件，而无需指定存储目录。这个方法会返回一个文件句柄，对于需要立即打开临时文件进行写入的场景很有用。同样，在使用完毕后也可以使用`fclose()`和`unlink()`函数来关闭和删除临时文件。

##参考链接
- PHP官方文档：[创建临时文件](https://www.php.net/manual/zh/function.tempnam.php)
- 《PHP设计模式与最佳实践》：[创建临时文件](https://design-patterns.readthedocs.io/zh_CN/latest/print_PDF/temporary_file.html)