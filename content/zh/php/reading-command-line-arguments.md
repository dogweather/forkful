---
title:                "PHP: 读取命令行参数"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# 为什么要阅读命令行参数？

在PHP编程中，阅读命令行参数是一项基本的技能。通过读取命令行参数，您可以为您的脚本提供外部输入，使其更加灵活和功能强大。例如，您可以根据命令行传递的参数来调整脚本的行为，或者让用户通过命令行输入数据。因此，学习如何读取命令行参数是非常重要的，它可以帮助您更好地掌握PHP编程知识，并实现更多有用的功能。

# 如何读取命令行参数

要读取命令行参数，首先需要声明一个 `$argv` 数组变量，它会包含所有通过命令行传递的参数。然后，您可以使用 `count()` 函数来检查参数的数量，或使用 `foreach` 循环来遍历所有的参数。下面是一个简单的示例，展示如何读取并输出命令行参数：

```PHP
<?php
// 声明 `$argv` 数组变量
$argv = $_SERVER['argv'];

// 检查参数的数量并输出结果
echo "您输入了 " . count($argv) . "个参数：\n";

// 使用 foreach 循环遍历所有参数并输出
foreach ($argv as $key => $value) {
    echo $key . ": " . $value . "\n";
}
```

假设您执行了以下命令：`php script.php hello world`，则上述示例的输出结果将为：

```
您输入了 3 个参数：
0: php
1: script.php
2: hello
3: world
```

您可以看到，第一个参数是操作PHP的二进制文件，第二个参数是您在命令行输入的脚本名称，剩下的参数则是您传递的实际参数。

# 深入学习命令行参数

除了基本的读取参数方法外，您还可以使用 `getopt()` 函数来解析命令行选项。`getopt()` 函数可以帮助您更轻松地处理多个参数，并将它们转换为可供使用的变量。您可以通过阅读更多资料来学习如何使用 `getopt()` 函数，并利用它来实现更复杂的命令行操作。

# 参考链接

- PHP官方文档：https://www.php.net/manual/zh/features.commandline.php
- 教程：https://www.w3cschool.cn/php/php-command-line.html
- 代码示例：https://www.runoob.com/php/php-commandline-input.html

# 另请参阅

- [阅读命令行参数的更多方法与技巧](https://www.example.com/read-command-line-arguments)
- [通过命令行参数来优化您的PHP脚本](https://www.example.com/optimize-php-script-with-command-line-arguments)