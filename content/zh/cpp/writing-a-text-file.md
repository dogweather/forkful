---
title:    "C++: 编写文本文件。"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 为什么

为什么会有人想要编写一个文本文件呢？一个简单的回答就是：文本文件是存储和共享信息的重要途径。通过编写文本文件，我们可以轻松地创建和修改文本内容，并将其与他人分享。

## 如何

如果你是一名C++程序员或者对编程有一定了解，那么你可能会想知道如何在C++中编写一个文本文件。下面我们将通过一个简单的示例来展示这个过程。

首先，我们需要在代码中包含 `fstream` 头文件来使用文件流。然后，我们可以使用 `ofstream` 对象来打开一个文件，并使用 `<<` 运算符将文本数据写入文件中。最后，我们需要使用 `close()` 函数关闭文件。

```
```C++
#include <fstream>
using namespace std;

int main() {
    // 创建一个名为"example.txt"的文本文件，并将其打开
    ofstream file("example.txt");
    
    // 将"Hello World!"写入文件中
    file << "Hello World!";
    
    // 关闭文件
    file.close();
    
    return 0;
}
```

运行这段代码后，你将在相同的目录下找到名为"example.txt"的文本文件，里面包含着我们写入的文本数据。

## 深入探讨

编写文本文件可能看起来很简单，但是其背后的流程和原理却并不简单。文本文件通常以ASCII码格式存储，每个字符都被编码为一个数字。因此，当我们将文本数据写入文件时，实际上是将ASCII码值存储在文件中。当我们打开文本文件进行阅读时，计算机会将ASCII码转换为字符，并将其显示给我们。

此外，我们还可以通过指定不同的打开模式来实现对文本文件的不同操作。比如，使用 `ios::app` 模式可以在文件末尾追加文本内容，使用 `ios::trunc` 模式可以清空文件内容，等等。

## 参考资料

- [C++ File Handling](https://www.geeksforgeeks.org/file-handling-c-classes/) (英文)
- [ASCII编码](https://www.asciitable.com/) (英文)

## 参见

- [C++中的文件输入输出操作](https://www.runoob.com/cplusplus/cpp-files-streams.html) (中文)
- [ASCII码表](https://blog.csdn.net/michaelhanlong/article/details/47105749) (中文)