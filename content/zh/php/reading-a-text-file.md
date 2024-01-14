---
title:    "PHP: 读取文本文件"
keywords: ["PHP"]
---

{{< edit_this_page >}}

# 为什么

阅读文本文件是每个程序员都需要掌握的基本技能。无论是读取用户输入、处理日志文件还是从服务器获取数据，都需要通过读取文本文件来获取信息。所以学习如何读取文本文件对于开发 PHP 应用程序来说至关重要。

# 如何读取文本文件

这里我们使用 PHP 内置的 `file()` 函数来读取文本文件。该函数将文件的每一行保存为一个数组元素，并返回包含所有行的数组。

**示例代码：**

```PHP
<?php
$file = file("test.txt"); // 读取文件 test.txt
foreach ($file as $line) { // 遍历每一行
    echo $line ."<br>"; // 输出每一行
}
?>
```

**示例输出：**

```
Hello world! 
This is a test file. 
It contains some random text.
```

除了使用 `file()` 函数，我们还可以使用 `fopen()` 和 `fgets()` 函数来逐行读取文本文件。

**示例代码：**

```PHP
<?php
$myfile = fopen("test.txt", "r"); // 打开文件 test.txt，使用 r 模式表示只读
while(!feof($myfile)) { // 判断是否已到达文件末尾
    echo fgets($myfile) . "<br>"; // 读取并输出每一行
}
fclose($myfile); // 关闭文件
?>
```

**示例输出：**

```
Hello world! 
This is a test file. 
It contains some random text.
```

# 深入了解

除了简单的文本文件，我们也可以读取和处理 CSV 文件、JSON 文件和 XML 文件等。PHP 也提供了相应的函数来处理这些特殊格式的文本文件。

- CSV 文件：使用 `fgetcsv()` 函数来读取 CSV 文件，并返回一个包含每一行数据的数组。
- JSON 文件：使用 `json_decode()` 函数来解码 JSON 文件，并返回一个包含 JSON 数据的数组。
- XML 文件：使用 `simplexml_load_file()` 函数来读取 XML 文件，并返回一个 XML 对象。我们也可以使用 `SimpleXML` 类来处理 XML 数据。

# 参考链接

- `file()` 函数文档：https://www.php.net/manual/zh/function.file.php
- `fopen()` 函数文档：https://www.php.net/manual/zh/function.fopen.php
- `fgets()` 函数文档：https://www.php.net/manual/zh/function.fgets.php
- `fgetcsv()` 函数文档：https://www.php.net/manual/zh/function.fgetcsv.php
- `json_decode()` 函数文档：https://www.php.net/manual/zh/function.json-decode.php
- `simplexml_load_file()` 函数文档：https://www.php.net/manual/zh/function.simplexml-load-file.php
- `SimpleXML` 类文档：https://www.php.net/manual/zh/book.simplexml.php