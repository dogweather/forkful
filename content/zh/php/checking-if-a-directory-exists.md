---
title:                "检查目录是否存在"
html_title:           "PHP: 检查目录是否存在"
simple_title:         "检查目录是否存在"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
检查目录是否存在是对服务器上某个路径是否包含指定目录的一种验证，适用于PHP开发环境。这是为了避免在尝试访问、更改或删除不存在的目录时产生错误。

## 如何操作：
使用 `is_dir()` 函数来检查目录是否存在。该函数返回布尔值：如果目录存在则返回 True，否则返回 False。

```PHP
<?php
$dir = "/path/to/my/dir";

if( is_dir($dir) ){
    echo "目录存在";
} else {
    echo "目录不存在";
}
?>
```
上面的代码将输出："目录存在" 或 "目录不存在"，取决于提供的路径是否存在。

## 深入探讨：
1. 关于历史背景: `is_dir()` 函数是PHP 4及更高版本中的内置函数，它提供了一种快速、简单的方法来验证一个目录是否存在。

2. 关于替代方案: 另一种可行的检查目录是否存在的方法是使用 `file_exists()` 函数。但是，注意它不仅检查目录，还检查文件。因此，如果你需要的是仅检查目录，使用 `is_dir()` 更合适。

```PHP
<?php
$dir = "/path/to/my/dir";

if( file_exists($dir) ){
    echo "目录或文件存在";
} else {
    echo "目录或文件不存在";
}
?>
```
3. 关于实现细节: 调用 `is_dir()` 函数时，会将目录作为字符串参数传入，然后该函数将返回一个布尔值来确认该目录在文件系统中是否存在。因此，确认路径字符串的正确性非常关键。

## 另请参阅：
- PHP官方文档 `is_dir()` 函数 参考:[PHP: is_dir - Manual](https://www.php.net/manual/en/function.is-dir.php)
- PHP官方文档 `file_exists()` 函数 参考:[PHP: file_exists - Manual](https://www.php.net/manual/en/function.file-exists.php)