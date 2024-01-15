---
title:                "检查目录是否存在"
html_title:           "C#: 检查目录是否存在"
simple_title:         "检查目录是否存在"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

为什么: 人们为什么要检查一个目录是否存在的最大2句解释？

一个常见的情况是，我们需要在程序中使用特定的目录来保存或读取文件，但是在运行程序之前，我们需要确保这个目录是否已经存在。因此，检查目录是否存在就显得非常重要。

如何: 以下是使用C#语言编写的示例代码和输出结果来演示如何检查目录是否存在的方法： 
```
C# // 导入System.IO库 using System.IO; // 指定要检查的目录路径 string directoryPath = @"C:\Users\Username\Documents\TestFolder"; // 使用Directory类中的Exists方法来检查目录是否存在 if (Directory.Exists(directoryPath)) { Console.WriteLine("目录已存在！"); } else { Console.WriteLine("目录不存在！"); } ``` 

如果目录“TestFolder”已经存在，输出结果将是“目录已存在！”；如果目录不存在，则输出结果将是“目录不存在！”。

深入了解: 要理解如何检查目录是否存在，我们需要了解以下两个概念：路径和Directory.Exists()方法。

路径是指文件或文件夹的地址，C#中的字符串变量用来存储路径信息，它们通常被包装在双引号中，并且可以包含反斜杠和其他特殊字符来表示路径中的空间等特性。

Directory.Exists()方法是C#中用来检查目录是否存在的方法，它接受一个字符串类型的参数来指定目录的路径，并且返回一个布尔值，如果目录存在，返回true，否则返回false。

除了使用Directory.Exists()方法来检查目录是否存在，我们还可以使用其他方法来完成相同的功能，例如使用File.Exists()方法来检查文件是否存在，或者使用Path.HasExtension()方法来检查路径是否有文件扩展名。

See Also (参考链接):

- C#中的路径和文件操作: https://www.w3schools.com/cs/cs_path.asp
- Directory Class (Directory类的官方文档): https://docs.microsoft.com/en-us/dotnet/api/system.io.directory?view=net-5.0