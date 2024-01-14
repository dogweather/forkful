---
title:    "C++: 读取文本文件"
keywords: ["C++"]
---

{{< edit_this_page >}}

##为什么

阅读文本文件是学习C++编程中的重要一步。通过阅读文本文件，您可以轻松地读取和处理大量的数据，从而提高程序的效率和功能性。因此，了解如何读取文本文件对于任何想要学习C++的人来说都是至关重要的。

##如何做

首先，您需要创建一个文本文件，并将其保存在您的程序所在的相同目录中。接下来，使用C++的输入输出流库，即iostream来打开文本文件，并从中读取数据。最后，使用循环语句和条件语句，“逐行”读取文本文件中的数据，并对其进行处理。下面是一个代码示例：

```C++
#include <iostream>
#include <fstream> //用于文件输入和输出的库

using namespace std;

int main() {
  ifstream file; //创建一个输入文件流
  file.open("my_text_file.txt"); //打开文本文件
  string data; //用于存储读取的数据的变量
  while (getline(file, data)) { //循环读取文本文件中的每一行
    if (data == "apple") { //条件语句用于处理特定的数据
      cout << "You like apples." << endl;
    }
  }
  file.close(); //关闭文件流
  return 0;
}
```

假设我们的文本文件中包含以下内容：

```
apple
orange
banana
```

上面的代码将输出以下内容：

```
You like apples.
```

##深入探讨

C++中的输入输出流库中有许多不同的函数可以用于读取文本文件，如`getline()`、`get()`等。另外，您还可以使用文件流对象的`eof()`函数来检查是否已经到达文件末尾。对于处理大量数据的程序来说，这些函数都非常有用。

##相关链接

- [C++输入输出流库教程](https://www.cplusplus.com/doc/tutorial/files/)
- [详解文件输入输出流](https://www.geeksforgeeks.org/file-handling-c-classes/)