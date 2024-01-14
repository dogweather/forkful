---
title:    "PHP: 检查目录是否存在。"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# 为什么要检查目录是否存在

当我们在进行编程时，有时候需要操作文件或者目录，但是在对文件或者目录进行操作之前，我们需要先确保它们存在。如果我们不先检查，可能会导致程序出错或者产生意想不到的结果。因此，检查目录是否存在是一种很重要的编程习惯。

# 如何检查目录是否存在

我们可以使用PHP中的`is_dir()`函数来检查目录是否存在。下面是一个简单的代码示例：

```PHP
<?php
// 检查当前目录是否存在
if (is_dir('.')) {
    echo "当前目录存在！";
} else {
    echo "当前目录不存在！";
}
```

运行结果：

```
当前目录存在！
```

除了使用`is_dir()`函数，我们也可以使用`file_exists()`函数来检查文件是否存在，或者使用`mkdir()`函数来创建目录。这些函数都需要了解一下，因为在日常编程中可能会经常用到。

# 深入了解检查目录是否存在

在PHP中，我们可以使用`stat()`函数来获取目录的一些详细信息。这个函数会返回一个数组，包含了目录的权限、所有者信息、最后修改时间等等。

```PHP
<?php
// 获取当前目录的详细信息
$info = stat('.');
// 打印数组
print_r($info);
```

运行结果：

```
Array
(
    [0] => 106
    [1] => 0
    [2] => 16877
    [3] => 1
    [4] => root
    [5] => root
    [6] => 0
    [7] => 0
    [8] => 1618255648
    [9] => 1618255648
    [10] => 1618255648
    [11] => 4096
    [12] => 8
    [dev] => 106
    [ino] => 0
    [mode] => 16877
    [nlink] => 1
    [uid] => 0
    [gid] => 0
    [rdev] => 0
    [size] => 4096
    [atime] => 1618255648
    [mtime] => 1618255648
    [ctime] => 1618255648
    [blksize] => 4096
    [blocks] => 8
)
```

通过深入了解目录的详细信息，我们可以更好地掌握和管理目录。

# 参考链接

- [PHP文档：`is_dir()`函数](https://www.php.net/manual/zh/function.is-dir.php)
- [PHP文档：`file_exists()`函数](https://www.php.net/manual/zh/function.file-exists.php)
- [PHP文档：`mkdir()`函数](https://www.php.net/manual/zh/function.mkdir.php)
- [PHP文档：`stat()`函数](https://www.php.net/manual/zh/function.stat.php)