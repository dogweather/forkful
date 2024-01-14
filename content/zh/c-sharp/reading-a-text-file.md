---
title:    "C#: 读取文本文件"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 为什么

阅读文本文件在程序设计中是一个非常常见的任务。文本文件包含了大量的信息，可以帮助我们更好地理解数据和调试代码。通过阅读文本文件，我们可以更有效地处理数据和提高程序的性能。

## 如何

首先，我们需要在代码中引入System.IO命名空间，这样我们才能使用C＃中读取文本文件的函数。然后，我们使用StreamReader类来打开文本文件并读取其内容，可以是一行或整个文件。最后，我们使用Close（）函数关闭文件，以防止文件被其他程序占用。

```C#
using System.IO;

// 打开文本文件
StreamReader file = new StreamReader("textfile.txt");

// 读取文件内容
string line = "";
while ((line = file.ReadLine()) != null)
{
    Console.WriteLine(line);
}

// 关闭文件
file.Close();
```

输出示例：

```
这是第一行文本。
这是第二行文本。
这是第三行文本。
```

## 深入探讨

在读取文本文件时，我们需要考虑文件的编码格式和换行符的差异。如果不指定编码格式，默认会使用操作系统的编码格式。如果文本文件中使用不同的编码格式，就可能导致乱码问题。因此，我们可以通过指定编码格式来确保文件正确解析。

另外，不同的操作系统也可能使用不同的换行符，Windows使用“\r\n”（回车换行），而Unix和Mac使用“\n”（换行）。为了避免出现读取错误，我们可以使用Environment.NewLine来代替硬编码换行符。

## 参考资料

- [C #文件操作（打开，创建，读写和删除文件）](https://codemeals.com/c-sharp/csharp-open-create-write-delete-files/)
- [C＃中读取文本文件的方法](https://www.c-sharpcorner.com/publication/reading-text-files-using-c-sharp)
- [C＃读取文本文件时指定编码](https://www.tutorialspoint.com/chash-reading-text-files-specified-encoding)
- [使用C＃读取文本文件的换行符问题](https://www.dotnetperls.com/environment-newline)

## 参见

- [C＃写入文本文件的方法（Mandarin）](https://myprogramwith.netlify.app/blog/how-to-write-to-a-text-file-in-c-sharp-mandarin/)