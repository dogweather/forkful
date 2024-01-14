---
title:    "PHP: 编写文本文件"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/php/writing-a-text-file.md"
---

{{< edit_this_page >}}

# 为什么
在PHP编程中，写入文本文件是一项非常常见且有用的任务。它允许我们以结构化的方式存储和检索数据，可以用来保存用户信息、日志记录、配置设置等等。无论是在网站开发还是数据处理中，写入文本文件都是一个非常有用的工具。

# 如何
要写入文本文件，我们需要用到PHP中的`fopen()`和`fwrite()`函数。首先，我们使用`fopen()`函数打开一个指定的文件路径，并指定文件的打开模式，例如"w"用于写入模式。然后，我们可以使用`fwrite()`函数将想要写入的数据以字符串的形式写入到文件中。最后，我们需要使用`fclose()`函数关闭文件指针，以确保写入的数据全部保存到文件中。

```PHP
$file = fopen("example.txt","w"); // 打开文件
fwrite($file, "这是要写入的文本。"); // 写入数据
fclose($file); // 关闭文件指针
```
运行以上代码后，我们将在同一目录下创建一个名为"example.txt"的文件，并将字符串"这是要写入的文本。"保存在文件中。

#深入探讨
除了基本的写入文本文件操作，我们还可以通过添加一些参数来进一步控制写入的方式。例如，通过指定文件打开模式为"a"，我们可以将数据追加到现有的文件末尾而不是覆盖原有内容；通过指定写入数据的长度，我们可以限制写入的字节数。此外，我们还可以通过使用`file_put_contents()`函数来简化上述三个步骤，它会自动打开、写入并关闭文件，并且可以同时支持多种写入模式。

# 参考文献
- [PHP官方文档: fopen()](https://www.php.net/manual/zh/function.fopen.php)
- [PHP官方文档: fwrite()](https://www.php.net/manual/zh/function.fwrite.php)
- [PHP官方文档: fclose()](https://www.php.net/manual/zh/function.fclose.php)
- [PHP官方文档: file_put_contents()](https://www.php.net/manual/zh/function.file-put-contents.php)

# 参见
- [PHP官方文档: 读取文本文件](https://www.php.net/manual/zh/function.fread.php)
- [PHP官方文档: 删除文本文件](https://www.php.net/manual/zh/function.unlink.php)