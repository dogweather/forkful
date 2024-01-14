---
title:    "C#: 检查目录是否存在"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

＃＃为什么
无论是为了确保程序的稳定性，还是为了提高代码的健壮性，检查目录是否存在都是非常重要的。通过此功能，您可以在代码运行时及时发现错误，并对其进行处理，从而大大提高了程序的可靠性。

＃＃如何进行检查
使用C#的Directory类的Exists方法可以轻松地检查目录是否存在。下面是一个简单的代码示例：

```
//创建一个目录路径的字符串
string path = @"C:\Users\Username\Documents\MyFolder"; 

//检查目录是否存在
if (Directory.Exists(path))
{
    Console.WriteLine("目录存在。");
}
else
{
    Console.WriteLine("目录不存在。");
}
```

上述代码将输出“目录存在。”如果该目录不存在，则会输出“目录不存在。”此外，您还可以使用try-catch代码块来捕获并处理任何异常。

＃＃深入了解
在C#中，检查目录是否存在是一个相对简单的功能。然而，当您涉及到更复杂的目录结构以及更多的限制条件时，它可能会变得复杂起来。因此，建议您在使用此功能时，要做好充分的异常处理，并考虑所有可能的情况。

＃＃了解更多
- [C# Directory.Exists方法文档]（https://docs.microsoft.com/zh-cn/dotnet/api/system.io.directory.exists?view=netframework-4.8）
- [C# try-catch代码块指南]（https://docs.microsoft.com/zh-cn/dotnet/csharp/language-reference/keywords/try-catch）
- [C#异常处理和错误处理指南]（https://docs.microsoft.com/zh-cn/dotnet/csharp/programming-guide/exceptions/）