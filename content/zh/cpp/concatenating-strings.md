---
title:                "串联字符串"
html_title:           "C++: 串联字符串"
simple_title:         "串联字符串"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## 为什么
当我们需要将多个字符串连接在一起时，我们可以使用字符串连接来创建一个新的字符串。这可以用于构建更复杂的输出，比如打印出一个完整的句子，或者拼接一个 URL 地址。

## 如何
使用 C++ 中的 `+` 运算符来完成字符串连接。下面是一个例子，展示了如何将两个字符串连接在一起并打印出结果：

```C++
#include <iostream>
using namespace std;

int main() {
    string first_name = "John";
    string last_name = "Doe";

    string full_name = first_name + " " + last_name;

    cout << full_name << endl;

    return 0;
}
```

输出结果为：
```
John Doe
```

这里我们使用`+`运算符将两个字符串 `first_name` 和 `last_name` 连接起来，并将结果赋值给 `full_name`。最后，我们使用 `cout` 来打印出完整的名字。

## 深入了解
除了使用`+`运算符外，我们还可以使用 `append()` 函数来拼接字符串。这个函数可以在一个已存在的字符串末尾添加新的内容。例如：

```C++
string sentence = "Today is a ";

sentence.append("great day!");

cout << sentence << endl;
```

输出结果为：
```
Today is a great day!
```

除了拼接字符串，我们也可以使用 `insert()` 函数来在指定位置插入新的内容。例如：

```C++
string sentence = "Today is a great day!";

sentence.insert(11, "very ");

cout << sentence << endl;
```

输出结果为：
```
Today is a very great day!
```

当我们需要在字符串中替换某些内容时，可以使用 `replace()` 函数。例如：

```C++
string sentence = "Today is a very great day!";

sentence.replace(11, 4, "not ");

cout << sentence << endl;
```

输出结果为：
```
Today is not a very great day!
```

现在，你已经知道了如何使用 `+` 运算符、`append()`、`insert()` 和 `replace()` 函数来进行字符串连接，可以尝试用这些方法来拼接不同的字符串，创造出自己想要的输出。

## 参考资料
- [C++ 字符串教程](https://www.runoob.com/cplusplus/cpp-strings.html)
- [C++ 字符串连接](https://www.geeksforgeeks.org/how-to-concatenate-two-strings-in-cpp/#:~:text=Strings%20can%20be%20concatenated%20using,in%20cpp%20%3D%20concatenation%20operator%20).)

## 参见
- [C++ 中的字符串函数](https://www.runoob.com/cplusplus/cpp-string-functions.html)
- [字符串连接示例](https://www.programiz.com/cpp-programming/examples/concatenate-strings)