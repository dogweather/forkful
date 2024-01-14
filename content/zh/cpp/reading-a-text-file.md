---
title:    "C++: 阅读文本文件"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

为什么:阅读文本文件的重要性只有1-2句话介绍。

在计算机编程中，文本文件是非常常用的数据格式。无论是存储文本数据还是读取文本数据，文本文件都扮演着非常重要的角色。 所以学习如何读取文本文件是非常有用的。

## 怎样进行

读取文本文件的过程有几个关键步骤：

1. 打开文件：使用 `fstream` 类来打开文件，这个类包含在标准库中。
2. 检查文件是否成功打开：使用 `is_open()` 函数来检查文件是否成功打开。
3. 读取数据：使用 `getline()` 函数来逐行读取文本文件中的数据。
4. 处理数据：使用其他合适的函数来处理所读取的数据。
5. 关闭文件：使用 `close()` 函数来关闭文件。

下面是一个例子，展示了如何打开、检查、读取和关闭一个文本文件中的数据：

```C++
#include <iostream>
#include <fstream>

using namespace std;

int main()
{
    string line;
    ifstream file("sample.txt"); // 打开文件

    if (file.is_open()) { // 检查文件是否成功打开
        while (getline(file, line)) { // 使用getline()逐行读取数据
            cout << line << endl; // 处理数据，输出到控制台
        }
        file.close();
    } else {
        cout << "文件打开失败！" << endl; // 打印错误信息
    }
    return 0;
}
```

假设我们有一个名为 "sample.txt" 的文本文件，里面的内容如下：

```
Hello
你好
Bonjour
```

上面的代码输出结果将会是：

```
Hello
你好
Bonjour
```

## 深入探讨

上面的代码只是一个简单的示例，如果想要更深入地了解如何读取文本文件，可以学习以下几点：

- 如何处理不同类型的文本数据：文本文件中可以包含不同类型的数据，包括数字、字符串等等。对应不同类型的数据，我们需要使用不同的函数来处理。
- 如何处理某一行中的特定内容：有时候我们可能只需要某一行中的某个特定内容，而不是整行的数据。这时候我们可以使用字符串操作函数来筛选出我们需要的内容。
- 如何将读取的文本数据存储到变量中：通过学习如何将文本文件中的数据存储到变量中，我们可以更方便地使用这些数据，并进行进一步的处理。

有了这些基础知识，我们就可以更加灵活地读取并处理文本文件中的数据了。

## 参考链接

- [C++ 文件/流 - 文本 I/O](https://www.runoob.com/cplusplus/cpp-files-streams.html)
- [C++ 字符串操作函数](https://www.runoob.com/cplusplus/cpp-string-functions.html)
- [C++ 数字操作函数](https://www.runoob.com/cplusplus/cpp-number-functions.html)

## 另请参阅

- [Markdown 语法手册](https://www.markdownguide.org/basic-syntax/)
- [C++ 数组操作指南](https://www.runoob.com/cplusplus/cpp-array-functions.html)