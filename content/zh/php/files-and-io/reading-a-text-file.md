---
date: 2024-01-20 17:54:56.363348-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C) \u4EE5\u524D\uFF0C\u6211\u4EEC\u53EF\
  \u80FD\u4F7F\u7528`fopen()`, `fgets()`, \u548C`fclose()`\u5B9E\u73B0\u6587\u4EF6\
  \u8BFB\u53D6\u3002\u73B0\u5728\uFF0C\u9664\u4E86`file_get_contents()`\u548C`file()`\uFF0C\
  \u8FD8\u53EF\u4EE5\u7528`SplFileObject`\uFF0C\u5B83\u63D0\u4F9B\u4E86\u9762\u5411\
  \u5BF9\u8C61\u7684\u6587\u4EF6\u64CD\u4F5C\u65B9\u6CD5\u3002\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:01.091580-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u64CD\u4F5C) \u4EE5\u524D\uFF0C\u6211\u4EEC\u53EF\u80FD\u4F7F\
  \u7528`fopen()`, `fgets()`, \u548C`fclose()`\u5B9E\u73B0\u6587\u4EF6\u8BFB\u53D6\
  \u3002\u73B0\u5728\uFF0C\u9664\u4E86`file_get_contents()`\u548C`file()`\uFF0C\u8FD8\
  \u53EF\u4EE5\u7528`SplFileObject`\uFF0C\u5B83\u63D0\u4F9B\u4E86\u9762\u5411\u5BF9\
  \u8C61\u7684\u6587\u4EF6\u64CD\u4F5C\u65B9\u6CD5\u3002"
title: "\u9605\u8BFB\u6587\u672C\u6587\u4EF6"
weight: 22
---

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
