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

# PHP 中检查目录是否存在

## 什么是检查目录是否存在以及为什么程序员要这么做？

检查目录是否存在是一种编程技巧，它可以让程序员判断某个目录是否存在于特定的文件系统中。程序员通常会使用这个技巧来确保程序在读取或操作一个目录之前，先确认它是否存在，从而避免可能发生的错误。

## 怎么做？

```PHP
// 检查一个目录是否存在
if (is_dir('/path/to/directory')) {
    echo '目录存在。';
}

// 创建一个新目录
mkdir('/path/to/new/directory');

// 不在提示错误
// 这在多次尝试创建同一个目录时会有用
mkdir('/path/to/new/directory', 0777, true);

// 列出一个目录中的所有文件和子目录的名称
$directory = '/path/to/directory';
if ($handle = opendir($directory)) {
    while (false !== ($entry = readdir($handle))) {
        if ($entry != '.' && $entry != '..') {
            echo $entry;
        }
    }
    closedir($handle);
}
```

输出：
```bash
目录存在。
somefile.txt
somedir
```

## 深入了解

1. 历史背景：检查目录是否存在这个技巧最早出现在UNIX系统中，目的是为了确保程序在执行系统调用时不会出错。
2. 其他方法：除了使用PHP自带的is_dir()函数来检查目录是否存在，程序员也可以使用file_exists()函数来检查文件或目录是否存在。
3. 实现细节：is_dir()函数在检查目录是否存在时，实际上是通过调用系统的stat()函数来获取目录的文件信息，再根据返回的信息来判断目录是否存在。

## 了解更多

- [PHP手册：is_dir()](https://www.php.net/manual/zh/function.is-dir.php)
- [PHP手册：file_exists()](https://www.php.net/manual/zh/function.file-exists.php)
- [Linux stat()函数文档](https://linux.die.net/man/2/stat)