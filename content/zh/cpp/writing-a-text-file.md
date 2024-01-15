---
title:                "编写文本文件"
html_title:           "C++: 编写文本文件"
simple_title:         "编写文本文件"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 为什么？

有时候，我们需要将数据保存在一个文件中，以便日后使用。通过编写文本文件，我们可以简单地将数据保存在可读性强的文本格式中，以便后续处理。

## 如何开始？

首先，我们需要包含头文件`<fstream>`，以便可以使用C++的文件输入输出功能。然后，我们需要定义一个`ofstream`对象来创建一个文件流，并打开需要写入的文件。接下来，我们可以使用`<<`符号来将数据写入文件，最后不要忘记关闭文件流。

```C++
#include <fstream>

// Create a file stream and open the file
ofstream file("example.txt");

// Write data to the file
file << "Hello, Mandarin readers! This is an example text file.";

// Close the file stream
file.close();
```

输出文件`example.txt`的内容如下所示：

```
Hello, Mandarin readers! This is an example text file.
```

## 深入探讨

当我们编写文本文件时，我们可以选择不同的打开模式，来控制文件流的行为。例如，如果我们希望重写已存在的文件，则可以使用`ofstream file("example.txt", ios::out)`，其中`ios::out`是写入模式的标识符。另外，如果我们希望每次写入数据都追加到文件末尾，可以使用`ios::app`标识。

除了使用`<<`符号来写入数据，我们还可以使用`put()`函数来写入单个字符，以及`write()`函数来写入一段内存中的数据。这些方法可以根据我们的实际需求来选择最合适的方式来写入文本文件。

## 参考资料

- 关于`ofstream`的更多信息，请参考[C++ Reference](https://en.cppreference.com/w/cpp/io/basic_fstream)
- 关于其他的文件输入输出标准库，请参考[C++ File I/O](https://www.geeksforgeeks.org/file-i-o-in-c/)
- 如果您对Markdown语法感兴趣，可以参考[Markdown 语法说明](https://www.markdown.xyz/basic-syntax/)