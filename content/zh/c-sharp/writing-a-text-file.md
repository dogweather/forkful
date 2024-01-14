---
title:    "C#: 编写文本文件"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 为什么

在编程中，写入和读取文本文件是一个常见的任务。通过编写文本文件，我们可以将数据保存并在需要的时候进行读取。此外，文本文件还可以与其他程序或用户共享，使数据的交流更加方便。

## 如何做

在C#中，写入文本文件可以使用StreamWriter类。首先，我们需要创建一个StreamWriter对象，并指定我们要写入的文件名和文件路径。然后，我们可以使用WriteLine方法来逐行向文件中写入内容。下面是一个简单的例子：

```C#
StreamWriter writer = new StreamWriter("test.txt");
writer.WriteLine("这是我要写入的第一行文本");
writer.WriteLine("这是我要写入的第二行文本");
writer.Close();
```

上面的代码会创建一个名为“test.txt”的文件，并向里面写入两行文本。最后，我们通过调用Close方法来关闭文件，确保数据被保存。

如果我们想要在写入文本的同时也在控制台输出，可以使用Console.WriteLine方法来实现，代码如下：

```C#
StreamWriter writer = new StreamWriter("test.txt");
Console.WriteLine("请输入要写入的内容：");
string input = Console.ReadLine();
writer.WriteLine(input);
writer.Close();
```

通过读取用户的输入，我们可以将其写入文本文件中。

## 深入了解

除了使用StreamWriter类外，C#中还有一些其他的方法可以使用来写入文本文件。例如，我们可以使用File类的WriteAllText方法来直接将所有内容写入文件，而不需要逐行写入。此外，我们还可以使用AppendAllText方法来追加内容到已有的文件中。

在写入文本文件时，我们需要注意文件的编码格式。通常情况下，C#默认会使用UTF-8编码来读写文本，但是也可以通过指定其他的编码格式来操作。例如，如果我们需要在文本中使用中文字符，可以使用Unicode编码来保证字符的正确显示。

## 参考资料

- C# StreamWriter类：https://docs.microsoft.com/zh-cn/dotnet/api/system.io.streamwriter?view=net-5.0
- C# File类：https://docs.microsoft.com/zh-cn/dotnet/api/system.io.file?view=net-5.0
- 关于文本编码的说明：https://docs.microsoft.com/zh-cn/dotnet/standard/base-types/character-encoding