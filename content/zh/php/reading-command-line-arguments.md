---
title:                "读取命令行参数"
html_title:           "PHP: 读取命令行参数"
simple_title:         "读取命令行参数"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why

有时候，作为一个PHP程序员，你可能需要从命令行读取参数以便在你的代码中做一些动态的操作。本文将帮助你了解如何在PHP中读取命令行参数，并给出一些示例来帮助你更好的理解。

## How To

首先，让我们看一个基本的例子来读取命令行参数。假设你有一个名为"hello.php"的PHP文件，并且希望从命令行获取一个用户输入的名字作为参数。

```PHP
// 从命令行读取名字参数
$name = $argv[1];
echo "你好，" . $name . "！";
```

现在，当你在命令行执行以下命令时：

```
php hello.php John
```

你将会得到以下输出：

```
你好，John！
```

你也可以通过使用 "argc" 变量来指定在命令行参数中传递的参数数量，这样可以防止未定义的参数索引错误。

```PHP
if ($argc < 2) {
    echo "请提供一个名字作为参数。";
}
else {
    $name = $argv[1];
    echo "你好，" . $name . "！";
}
```

上面的代码将在没有传入参数时给出提示，而在传入参数后则会输出相应的欢迎语。

## Deep Dive

除了基本的命令行参数读取外，PHP也提供了一些内置函数来帮助我们更有效地读取和处理命令行参数。

- `getopt()` 函数可以帮助我们解析复杂的命令行选项，并得到一个关联数组。
- `getopt()` 函数的第三个参数可以用来指定需要接收哪些参数，可以是字符串、数组或者字符串数组。
- 在PHP 8.0及以上版本中，我们也可以使用 "named arguments" 来处理命令行参数。

更多关于命令行参数的深入内容，可以参考PHP官方文档。

## See Also

- [PHP官方文档 - 从命令行获取参数]()
- [PHP官方文档 - 命令行选项解析]()
- [PHP官方文档 - 命令行选项命名参数]()