---
title:                "C++: 读取文本文件"
simple_title:         "读取文本文件"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

#为什么

阅读文本文件在编程中是一个重要的技能。它可以帮助我们在程序中读取和处理大量的文本数据，使我们的代码更高效、更有效。同时，理解如何读取文本文件也可以帮助我们更好地理解计算机编码的工作原理，从而提升我们作为程序员的技能水平。

#如何做

在C++中，我们可以使用文件流类（fstream）来读取文本文件。首先，我们需要包含头文件`<fstream>`。然后，我们需要创建一个文件流对象，并打开我们想要读取的文本文件。接下来，我们可以使用`ifstream`类中的`getline()`函数来逐行读取文本文件中的内容。最后，记得关闭文件流对象，以释放资源。

```
#include <iostream>
#include <fstream>

using namespace std;

int main() {
    //创建文件流对象并打开文件
    ifstream file("text.txt");

    //读取文件中的每一行并输出到屏幕上
    string line;
    while (getline(file, line)) {
        cout << line << endl;
    }

    //关闭文件流对象
    file.close();

    return 0;
}
```

假设我们有一个名为`text.txt`的文本文件，其中包含以下内容：

```
Hello
World
```

以上代码输出的结果将会是：

```
Hello
World
```

#深入了解

除了使用`getline()`函数来逐行读取文本文件，还有其他一些方法可以读取文本文件中的内容。比如，我们可以使用`get()`函数来逐个字符地读取文本文件中的内容，或者使用`read()`函数来读取指定长度的文本。

此外，我们也可以通过设置文件流对象的定位指针来读取文本文件中的指定行或指定位置的内容。这些方法的具体实现方式可以参考C++的官方文档或其他编程技术网站上的相关教程。

#另请参阅

- [C++文件流类](https://zh.cppreference.com/w/cpp/io/basic_ifstream)
- [C++文本文件读写操作](https://www.runoob.com/cplusplus/cpp-files-streams.html)
- [C++标准库头文件fstream的使用](https://www.jianshu.com/p/d7c14f8b07a3)