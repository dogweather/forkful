---
title:                "PHP: 读取文本文件"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 为什么要阅读文本文件？

阅读文本文件是每个程序员都会遇到的基本任务。文本文件中存储着大量的数据，比如配置文件、日志文件、文本记录等等。通过阅读这些文本文件，可以获取重要的数据，进行数据分析和处理，从而更好地编写代码。

## 如何阅读文本文件

阅读文本文件的方法很简单，只需要使用PHP的内置函数`file_get_contents()`即可。这个函数可以将文本文件的内容读取出来，并作为一个字符串返回。下面是一个简单的例子，演示如何读取一个文本文件：

```PHP
$file_content = file_get_contents("text_file.txt");
echo $file_content;
```

上面的例子中，我们使用了`file_get_contents()`函数读取了名为`text_file.txt`的文本文件，并将其内容存储在变量`$file_content`中。然后我们使用`echo`语句打印出了读取到的文件内容。

## 深入了解阅读文本文件

除了使用`file_get_contents()`函数，我们还可以使用其他的一些函数来读取文本文件。比如`fopen()`函数可以打开一个文本文件，`fgets()`函数可以逐行读取文件内容，`fclose()`函数可以关闭打开的文件句柄。深入了解这些函数的使用方法，有助于更灵活地读取文本文件，并处理其中的数据。

## 参考链接

- PHP `file_get_contents()`文档：https://www.php.net/manual/zh/function.file-get-contents.php
- PHP `fopen()`文档：https://www.php.net/manual/zh/function.fopen.php
- PHP `fgets()`文档：https://www.php.net/manual/zh/function.fgets.php
- PHP `fclose()`文档：https://www.php.net/manual/zh/function.fclose.php