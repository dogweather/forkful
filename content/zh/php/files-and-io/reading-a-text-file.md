---
date: 2024-01-20 17:54:56.363348-07:00
description: "\u5728PHP\u4E2D\u8BFB\u6587\u672C\u6587\u4EF6\uFF0C\u5C31\u662F\u5C06\
  \u6587\u4EF6\u5185\u5BB9\u52A0\u8F7D\u8FDB\u5185\u5B58\u3002\u8FD9\u901A\u5E38\u7528\
  \u4E8E\u6570\u636E\u8BFB\u53D6\u3001\u914D\u7F6E\u52A0\u8F7D\u7B49\u3002\u4E3A\u4EC0\
  \u4E48\u9700\u8981\uFF1F\u56E0\u4E3A\u7A0B\u5E8F\u8981\u7528\u6570\u636E\uFF0C\u800C\
  \u4E14\u7ECF\u5E38\u9700\u8981\u5904\u7406\u5B58\u50A8\u5728\u6587\u4EF6\u4E2D\u7684\
  \u6570\u636E\u3002"
isCJKLanguage: true
lastmod: '2024-02-25T18:49:45.451409-07:00'
model: gpt-4-1106-preview
summary: "\u5728PHP\u4E2D\u8BFB\u6587\u672C\u6587\u4EF6\uFF0C\u5C31\u662F\u5C06\u6587\
  \u4EF6\u5185\u5BB9\u52A0\u8F7D\u8FDB\u5185\u5B58\u3002\u8FD9\u901A\u5E38\u7528\u4E8E\
  \u6570\u636E\u8BFB\u53D6\u3001\u914D\u7F6E\u52A0\u8F7D\u7B49\u3002\u4E3A\u4EC0\u4E48\
  \u9700\u8981\uFF1F\u56E0\u4E3A\u7A0B\u5E8F\u8981\u7528\u6570\u636E\uFF0C\u800C\u4E14\
  \u7ECF\u5E38\u9700\u8981\u5904\u7406\u5B58\u50A8\u5728\u6587\u4EF6\u4E2D\u7684\u6570\
  \u636E\u3002"
title: "\u9605\u8BFB\u6587\u672C\u6587\u4EF6"
---

{{< edit_this_page >}}

## What & Why? (是什么 & 为什么?)
在PHP中读文本文件，就是将文件内容加载进内存。这通常用于数据读取、配置加载等。为什么需要？因为程序要用数据，而且经常需要处理存储在文件中的数据。

## How to: (如何操作)
```PHP
<?php
// 读取整个文件到一个字符串
$content = file_get_contents("example.txt");
echo $content;

// 逐行读取文件
$file = new SplFileObject("example.txt");
while (!$file->eof()) {
    echo $file->fgets();
}

// 使用file()函数读取文件到数组每行一项
$lines = file("example.txt", FILE_IGNORE_NEW_LINES);
foreach ($lines as $line) {
    echo $line . PHP_EOL;
}
?>
```

输出取决于"example.txt"的内容。

## Deep Dive (深度剖析)
以前，我们可能使用`fopen()`, `fgets()`, 和`fclose()`实现文件读取。现在，除了`file_get_contents()`和`file()`，还可以用`SplFileObject`，它提供了面向对象的文件操作方法。

同样重要的是处理异常－文件可能不存在或读取错误。PHP提供`try...catch`结构以优雅地处理这些情况。

```PHP
<?php
try {
    $content = file_get_contents("missing.txt");
} catch (Exception $e) {
    echo "Error: " . $e->getMessage();
}
?>
```

替代方法：在Linux系统中，也可以使用命令行工具如`cat`通过`exec()`读取文件，但在Web应用中通常不推荐。

## See Also (另请参阅)
- PHP官方文档：[file_get_contents](https://www.php.net/manual/en/function.file-get-contents.php)
- PHP官方文档：[SplFileObject](https://www.php.net/manual/en/class.splfileobject.php)
- PHP异常处理：[Exceptions](https://www.php.net/manual/en/language.exceptions.php)
