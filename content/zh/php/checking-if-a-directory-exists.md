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

# 为什么要检查目录是否存在？

在编写PHP程序时，经常需要处理文件和目录。而在访问目录之前，我们需要先确认该目录是否存在。这样可以避免因为目录不存在而导致程序崩溃或出现意外情况。

## 如何检查目录是否存在？

使用PHP的内置函数`is_dir()`可以判断一个路径是否为目录。下面是一个简单的例子：

```PHP
<?php
$directory = "/var/www/html"; // 被检查的目录
if (is_dir($directory)) { // 使用 is_dir() 函数进行判断
    echo "目录存在！"; // 如果返回 true，则说明目录存在
} else {
    echo "目录不存在！"; // 如果返回 false，则说明目录不存在
}
?>
```

输出结果将会是 `目录存在！`，因为我们检查的目录确实存在。

## 深入了解

除了使用`is_dir()`函数，我们还可以使用`file_exists()`和`is_file()`函数来检查文件和目录的存在性。不同的是，`file_exists()`可以用于检查文件或目录，而`is_file()`仅用于检查文件。下面是一个使用`file_exists()`函数的例子：

```PHP
<?php
$directory = "/var/www/html"; // 被检查的目录
if (file_exists($directory)) { // 使用 file_exists() 函数进行判断
    echo "目录存在！"; // 如果返回 true，则说明目录存在
} else {
    echo "目录不存在！"; // 如果返回 false，则说明目录不存在
}
?>
```

输出结果同样是 `目录存在！`。

# 查看其他信息

- [PHP手册：`is_dir()`函数](https://www.php.net/manual/zh/function.is-dir.php)
- [PHP手册：`file_exists()`函数](https://www.php.net/manual/zh/function.file-exists.php)
- [PHP手册：`is_file()`函数](https://www.php.net/manual/zh/function.is-file.php)

# See Also 

# 参考链接 

- [PHP Manual: `is_dir()` function](https://www.php.net/manual/en/function.is-dir.php)
- [PHP Manual: `file_exists()` function](https://www.php.net/manual/en/function.file-exists.php)
- [PHP Manual: `is_file()` function](https://www.php.net/manual/en/function.is-file.php)