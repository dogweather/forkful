---
title:                "C#: 编写文本文件"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

### 为什么要写文本文件

在程序开发中，文本文件是一种非常有用的工具。通过写入文本文件，我们可以保存和传输数据，保留程序的运行记录和输出结果。文本文件也可以作为程序之间通信的一种方式，使得不同的程序能够共享数据。因此，学习如何写文本文件是编程中必不可少的一部分。

### 如何写文本文件

在C＃中，要写入文本文件，我们需要使用StreamWriter类。首先，我们需要在程序中引入命名空间System.IO，这个命名空间包含了我们需要用到的类。

接下来，我们需要创建一个StreamWriter对象，并指定要写入的文本文件的路径和文件名。然后，使用WriteLine方法向文件中逐行写入文本。最后，我们需要使用Close方法关闭文件。

```C#
using System.IO;

// 创建StreamWriter对象，并指定要写入的文本文件的路径和文件名
StreamWriter writer = new StreamWriter("C:\\Users\\User\\Desktop\\sample.txt");
// 写入文本
writer.WriteLine("这是第一行文本");
writer.WriteLine("这是第二行文本");
// 关闭文件
writer.Close();
```

如果我们想要写入的文本是变量中的内容，可以使用Write方法，它可以接受变量作为参数。

```C#
int age = 25;
writer.Write("我的年龄是：" + age);
```

最后，我们可以通过使用StreamReader类来读取我们刚刚写入的文本文件，以确保数据已经成功写入。

### 深入了解写入文本文件

除了使用StreamWriter类之外，我们还可以使用File类来写入文本文件。File类提供了一系列静态方法，可以让我们更方便地操作文件。例如，我们可以使用File.WriteAllText方法来直接将文本写入到指定的文件中。

```C#
File.WriteAllText("C:\\Users\\User\\Desktop\\sample2.txt", "这是一段文本");
```

另外，我们还可以在写入文本文件的同时，指定文本编码格式。如果我们需要向文件中写入非ASCII字符，就需要使用特定的编码格式，比如UTF-8。

```C#
File.WriteAllText("C:\\Users\\User\\Desktop\\sample3.txt", "这是一段文本", Encoding.UTF8);
```

总的来说，写入文本文件是编程中一个基础但是非常重要的技能。通过使用文本文件，我们可以方便地保存和传输数据，也可以实现程序之间的通信。希望这篇文章对你有所帮助！

### 参考链接

- [C#文本文件写入教程](https://www.runoob.com/csharp/csharp-file-write.html)
- [C#文本文件读取教程](https://www.runoob.com/csharp/csharp-file-read.html)
- [C#超级入门指南（文本文件部分）](https://www.yuque.com/ohx7p7/hgvv6y/broihc)