---
title:                "PHP: 判断目录是否存在"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# 为什么要检查目录是否存在？

作为一名PHP程序员，你可能经常需要在代码中使用文件和目录来存储和管理数据。但在访问这些文件和目录之前，我们需要确认它们是否存在。这就是为什么检查目录是否存在如此重要的原因。如果我们不检查目录是否存在，可能会导致程序崩溃或无法正常工作。

# 如何检查目录是否存在？

在PHP中，我们可以使用`file_exists()`函数来检查目录是否存在。让我们看一个简单的例子：

```PHP
<?php
$directory = "my_directory";
if (file_exists($directory)) {
    echo "目录存在！";
} else {
    echo "目录不存在！";
}
```

如果`my_directory`目录存在，输出将是“目录存在！”。如果目录不存在，输出将是“目录不存在！”。通过这种方法，我们可以轻松检查目录是否存在，并根据情况采取相应的操作。

# 深入了解检查目录是否存在

有时候，我们可能需要检查目录是否为空。为了做到这一点，我们可以使用`scandir()`函数来获取目录中的所有文件和子目录，然后使用`count()`函数来计算数量。如果目录为空，计数将为0。让我们看一个例子：

```PHP
<?php
$directory = "my_directory";
$files = scandir($directory);
if (count($files) == 0) {
    echo "目录为空！";
} else {
    echo "目录不为空！";
}
```

除了使用`file_exists()`和`scandir()`函数，我们还可以使用`is_dir()`函数来检查目录是否存在。这个函数会返回一个布尔值，如果目录存在则为`true`，不存在则为`false`。让我们看一个例子：

```PHP
<?php
$directory = "my_directory";
if (is_dir($directory)) {
    echo "目录存在！";
} else {
    echo "目录不存在！";
}
```

# 参考阅读

- `file_exists()`函数：https://www.php.net/manual/zh/function.file-exists.php
- `scandir()`函数：https://www.php.net/manual/zh/function.scandir.php
- `count()`函数：https://www.php.net/manual/zh/function.count.php
- `is_dir()`函数：https://www.php.net/manual/zh/function.is-dir.php

# 参见

Simplified Chinese Markdown：https://docs.github.com/cn/github/writing-on-github/getting-started-with-writing-and-formatting-on-github/basic-writing-and-formatting-syntax#task-lists