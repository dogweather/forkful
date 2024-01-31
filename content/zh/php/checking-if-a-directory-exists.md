---
title:                "检查目录是否存在"
date:                  2024-01-20T14:57:49.678119-07:00
html_title:           "Elixir: 检查目录是否存在"
simple_title:         "检查目录是否存在"

category:             "PHP"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
检查目录是否存在是确认特定文件夹是否在文件系统中的过程。程序员这样做为了避免读写不存在的目录错误，确保数据保存到正确的位置。

## 如何操作：
```PHP
<?php
// 检查某个目录是否存在
$directory = "/path/to/directory";

if (is_dir($directory)) {
    echo "目录存在。";
} else {
    echo "目录不存在。";
    // 可以选择创建目录
    // mkdir($directory);
}
?>
```
**样本输出：**
```
目录存在。
```
或者如果目录不存在：
```
目录不存在。
```

## 深入探究
在PHP的早期版本中，`is_dir`和其他文件系统函数是开发者管理文件和目录的主要工具。虽然`is_dir`依然是检查目录存在性的标准方式，现在也有了如SPL（标准PHP库）提供的面向对象的替代方案。例如，`SplFileInfo`类提供了方法来处理文件和目录。

实现细节方面，`is_dir`在底层会调用相应平台的文件系统API来确定路径是否指向一个目录。如果在创建目录时可能存在并发问题（比如同一时间有多个脚本尝试创建同一个目录），开发者需要使用更复杂的逻辑来处理这种情况。

## 参见
- [PHP官方文档: is_dir](https://www.php.net/manual/zh/function.is-dir.php)
- [PHP官方文档: mkdir](https://www.php.net/manual/zh/function.mkdir.php)
- [PHP官方文档: SplFileInfo](https://www.php.net/manual/zh/class.splfileinfo.php)
