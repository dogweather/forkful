---
title:                "C++: 编写一个文本文件"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/writing-a-text-file.md"
---

{{< edit_this_page >}}

# 为什么会有写文本文件的需求？

作为一个程序员，你可能会觉得编写文本文件没有太大的意义，因为我们通常更倾向于编写代码来完成重要的任务。但是写文本文件也同样是一个很重要的技能，因为它能够帮助我们存储和保存数据，或者与其他程序共享信息。在这篇博客文章中，我将向大家介绍如何使用C++语言来编写文本文件。

## 如何编写文本文件

首先，我们需要包含头文件`<fstream>`来使用文件流的相关功能。在代码中使用`ofstream`来创建一个输出文件流，并指定要创建的文件的名称。比如，在这个例子中，我将创建一个名为“myfile.txt”的文本文件。

```C++
#include <fstream>

ofstream myfile("myfile.txt");
```

接下来，我们可以通过使用`<<`操作符来向文件中写入数据。比如，我们可以使用`<<`操作符来向文件中写入字符串或数字。下面的例子演示了如何向文件中写入"Hello World!"这个字符串。

```C++
myfile << "Hello World!";
```

如果我们想要向文件中写入多行内容，我们可以使用`endl`来表示换行符。下面的例子演示了如何向文件中写入多行内容。

```C++
myfile << "Hello World!" << endl;
myfile << "This is a sample text file." << endl;
myfile << "I love programming!" << endl;
```

最后，我们需要记得在我们结束向文件中写入数据后，关闭文件流。这可以通过调用`close()`函数来完成。

```C++
myfile.close();
```

当我们完成以上步骤后，就成功地创建了一个名为"myfile.txt"的文本文件，并向文件中写入了我们指定的内容。当我们打开这个文件时，会看到里面包含我们写入的内容。

## 深入了解文本文件

在C++中，文本文件是以ASCII或Unicode编码来存储的。当我们写入字符串或数字时，它们会被以ASCII或Unicode编码的形式写入到文件中。当我们打开文件时，文本编辑器会根据文件的编码将这些内容解析为可读的字符。

另外，如果我们想要在文本文件中添加新的内容，我们可以使用`ios::app`参数来打开文件流。这将允许我们在文件的末尾继续写入数据，而不是重写已有的内容。下面的代码演示了如何添加新的内容到已有的文件中。

```C++
ofstream myfile("myfile.txt", ios::app);
```

当我们想要读取文本文件中的内容时，我们可以使用`ifstream`来创建一个输入文件流，并使用`>>`操作符来读取数据。下面的代码演示了如何从文件中读取一行内容并将其打印出来。

```C++
ifstream myfile("myfile.txt");
string line;
myfile >> line;
cout << line;
```

这些都是编写文本文件的基础知识，希望通过这篇文章能够帮助大家更好地了解和使用C++来处理文本文件。

# 参考链接

- [C++文本文件操作](https://www.runoob.com/cplusplus/cplusplus-files-streams.html)
- [文本文件的读写操作](https://www.bogotobogo.com/cplusplus/fstream_input_output.php)