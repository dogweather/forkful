---
title:                "C#: 读取文本文件"
simple_title:         "读取文本文件"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

#为什么：阅读文本文件的目的

当你在编写C#程序时，有可能需要从外部文本文件中读取数据。这样可以方便地在程序中使用外部数据，而不是手动输入或硬编码在程序中。通过阅读文本文件，你可以快速、轻松地获取所需的数据，从而提升程序的效率和准确性。

##如何做到：使用C#读取文本文件的代码示例

为了读取文本文件，我们首先需要使用C#中的`StreamReader`类。以下是一个简单的代码示例，展示了如何使用`StreamReader`类来打开、读取和关闭文本文件。

```C#
//使用StreamReader打开文本文件
StreamReader file = new StreamReader("sample.txt");

//读取文本文件的内容并存储在一个变量中
string content = file.ReadToEnd();

//打印文本文件的内容
Console.WriteLine(content);

//关闭文本文件
file.Close();
```

在上面的代码中，我们首先创建了一个`StreamReader`对象并传入要读取的文本文件的名称作为参数。然后，我们使用`ReadToEnd()`方法将文本文件的所有内容读取并存储在一个字符串变量中。最后，我们使用`Console.WriteLine()`方法打印文本文件的内容，并用`Close()`方法关闭`StreamReader`对象。

以下是`sample.txt`文本文件的示例内容：

```
Hello world!
This is a sample text file.
```

当我们运行上面的代码时，屏幕上将会显示出文本文件中的内容：

```
Hello world!
This is a sample text file.
```

##深入了解：文本文件的读取过程

了解如何使用C#读取文本文件是编程中的基础知识，但如果你想进一步深入了解，还有几个方面值得注意。

首先，你需要确保指定文本文件的正确路径。如果你不确定文本文件的确切位置，可以在代码中使用相对路径或绝对路径来打开文件。

其次，C#中的`StreamReader`类还提供了其他一些有用的方法，如`ReadLine()`和`Peek()`。前者可以逐行读取文本文件的内容，而后者可以查看文件的下一行内容而不移动文件指针。

另外，当你读取文本文件时，需要注意编码格式。如果文本文件使用的是特殊的编码格式（如UTF-8），你需要在`StreamReader`对象的初始化中指定该编码。

综上所述，通过深入了解文本文件的读取过程，你可以更加灵活地应用于实际的编程工作中。

#另请参阅

- [Microsoft文档：StreamReader类](https://docs.microsoft.com/zh-cn/dotnet/api/system.io.streamreader?view=net-5.0)
- [C#文本文件的读取与写入](https://www.runoob.com/csharp/csharp-read-write-text-files.html)
- [如何使用C#读取文本文件](https://www.learnhowtoprogram.com/introduction-to-programming/collections-and-generics-6ba7b51f-9ea5-46c2-9819-e2918ea0338d/file-io/how-to-read-a-text-file)