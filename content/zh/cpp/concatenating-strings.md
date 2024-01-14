---
title:    "C++: 连接字符串"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## 为什么

拼接字符串在C++编程中是一个非常常见的任务。它可以让你将多个短字符串连接起来，形成一个更长的字符串。这对于创建动态文本内容以及格式化输出都非常有用。

## 如何做

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    // 创建两个字符串变量
    string str1 = "你好";
    string str2 = "世界";

    // 使用"+"操作符拼接字符串
    string combinedString = str1 + str2;

    // 输出结果
    cout << combinedString << endl;
}

// 输出结果: 你好世界
```

在这个简单的示例中，我们使用了`string`标准库以及"+"操作符来将两个字符串变量连接起来。首先，我们创建了两个字符串变量`str1`和`str2`，分别赋值为"你好"和"世界"。然后，我们使用"+"操作符将这两个变量连接起来，并将结果赋值给新的字符串变量`combinedString`。最后，我们使用`cout`语句将`combinedString`输出到控制台。

这只是一个简单的例子，拼接字符串的方式有很多种，具体取决于你的需求。你也可以试试使用其他操作符或者使用标准库中提供的函数来拼接字符串。

## 深入探讨

拼接字符串不仅仅是将两个字符串连接在一起，它还可以用于处理动态文本内容。例如，你可以将用户输入的不同字符串拼接起来，形成一个新的字符串来响应用户的操作。此外，拼接字符串也可以用于格式化输出，比如将变量和字符串连接在一起来生成一段信息。

当然，拼接字符串也有一些缺点。首先，每次拼接字符串都会创建一个新的字符串对象，这样会增加程序的复杂性和占用更多的内存。其次，拼接字符串操作也会影响程序的性能。如果你需要多次拼接大量的字符串，建议使用`stringstream`标准库来替代拼接，它会更高效。

## 参考链接

- [C++ string concatenation](https://www.geeksforgeeks.org/how-to-concatenate-two-strings-in-cpp/#:~:text=Concatenation%20of%20strings%20is%20a,used%20to%20concatenate%20two%20strings.)
- [C++ stringstream](https://www.cplusplus.com/reference/sstream/stringstream/)