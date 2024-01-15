---
title:                "创建临时文件"
html_title:           "C#: 创建临时文件"
simple_title:         "创建临时文件"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 为什么

创建临时文件可能是在处理大量数据时的一种手段，也可以用于存储程序运行过程中产生的临时数据。这样可以减少内存占用和提高性能。

## 如何

为了创建一个临时文件，我们可以使用C#中的`System.IO.Path`类来生成一个随机的文件名，然后将它与需要存储的数据一起传递给`System.IO.File.Create()`方法。最后，在使用完文件后，记得删除它，以节省磁盘空间。以下是一个简单的示例代码：

```C#
// 生成随机文件名
string fileName = Path.GetRandomFileName();
// 将文件名与路径拼接
string filePath = Path.Combine(Path.GetTempPath(), fileName);
// 创建临时文件
FileStream tempFile = File.Create(filePath);
// 写入数据到临时文件
byte[] data = { 1, 2, 3 };
tempFile.Write(data, 0, data.Length);
// 关闭文件流并删除文件
tempFile.Close();
File.Delete(filePath);
```

上述代码会在系统的临时文件夹中创建一个临时文件，并向其写入一组数据，最后将其删除。

## 深入了解

除了上面提到的方法，C#还提供了更多处理临时文件的选项。例如，可以使用`System.IO.Path.GetTempFileName()`方法直接获取一个临时文件名，而不需要生成随机文件名。还可以使用`System.IO.Path.GetTempPath()`方法来获取临时文件夹的路径。此外，还可以使用`System.IO.Path.GetTempPath()`方法来指定临时文件的属性，如是否允许其他进程访问。

## 参考链接

- [Microsoft Docs: File.Create Method](https://docs.microsoft.com/en-us/dotnet/api/system.io.file.create?view=net-5.0)
- [Microsoft Docs: Path.GetTempFileName Method](https://docs.microsoft.com/en-us/dotnet/api/system.io.path.gettempfilename?view=net-5.0)
- [Microsoft Docs: Path.GetTempPath Method](https://docs.microsoft.com/en-us/dotnet/api/system.io.path.gettemppath?view=net-5.0)