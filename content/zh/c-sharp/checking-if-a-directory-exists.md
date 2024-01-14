---
title:                "C#: 检查目录是否存在。"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 为什么

有时候在编程过程中，我们需要检查某个特定目录是否存在。这可以帮助我们避免出错或者优化我们的代码。接下来我们将学习如何通过C#代码来检查目录是否存在。

## 如何做

我们可以使用`Directory.Exists()`方法来检查目录是否存在。下面是一个简单的示例代码：

```C#
if(Directory.Exists("C:\\myDirectory")) 
{
    // 如果目录存在，执行这里的代码
    Console.WriteLine("myDirectory存在!");
} 
else 
{
    // 如果目录不存在，执行这里的代码
    Console.WriteLine("myDirectory不存在!");
}
```

上述代码将会先检查"C:\\myDirectory"是否存在，在代码中进行相应的输出。如果目录存在，将会输出"myDirectory存在!"，如果目录不存在，将会输出"myDirectory不存在!"。

## 深入探讨

在深入探讨检查目录是否存在的过程中，我们需要了解`Directory.Exists()`方法的工作原理。该方法将会返回一个布尔值，表示目录是否存在。如果目录存在，则返回`true`，如果目录不存在，则返回`false`。

同时，我们还可以使用`FileAttributes.Directory`来检查目录的属性，以确定它是否为一个目录而不是一个文件。在这种情况下，我们不仅需要检查目录是否存在，还需要确认它是一个目录。

## 参考链接

- [MSDN 文档：Directory.Exists 方法（System.IO）](https://msdn.microsoft.com/zh-cn/library/system.io.directory.exists(v=vs.110).aspx)
- [MSDN 文档：FileAttributes 枚举](https://msdn.microsoft.com/zh-cn/library/system.io.fileattributes(v=vs.110).aspx)
- [C# 目录和文件操作教程](https://www.tutorialspoint.com/csharp/csharp_directories.htm)


---

## 参考链接

- [MSDN Documentation: Directory.Exists Method (System.IO)](https://msdn.microsoft.com/en-us/library/system.io.directory.exists(v=vs.110).aspx)
- [MSDN Documentation: FileAttributes Enumeration](https://msdn.microsoft.com/en-us/library/system.io.fileattributes(v=vs.110).aspx)
- [C# Directory and File Operations Tutorial](https://www.tutorialspoint.com/csharp/csharp_directories.htm)