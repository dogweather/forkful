---
title:    "C++: 计算字符串长度。"
keywords: ["C++"]
---

{{< edit_this_page >}}

## 为什么

字符串是我们在编程中经常会遇到的一种数据类型，它不仅可以存储文字，还可以用来表示其他数据。在处理字符串的过程中，有时我们需要知道字符串的长度，这样才能更有效地对其进行操作。因此，找到字符串的长度是很有用的，让我们来看看如何做到这一点。

## 如何

在C ++中，可以通过使用 `strlen` 函数来找到字符串的长度。我们先来看一个简单的例子：

```C++
#include <iostream>

using namespace std;

int main() {
    char str[] = "Hello World";
    int length = strlen(str);
    cout << "The length of the string is: " << length << endl;
    return 0;
}
```

这段代码中，我们定义了一个字符数组 `str` 来存储字符串 "Hello World"。接下来，我们使用 `strlen` 函数来计算字符串的长度，并将结果赋值给 `length` 变量。最后，我们使用 `cout` 来输出结果。运行程序，我们可以看到控制台打印出字符串的长度为11。

除了使用 `strlen` 函数，我们还可以使用字符串类中的 `length` 方法来获取字符串的长度。像这样：

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    string str = "Hello World";
    int length = str.length();
    cout << "The length of the string is: " << length << endl;
    return 0;
}
```

这个例子中，我们使用了 `string` 类来定义字符串 `str`。然后，我们使用 `length` 方法来获取字符串的长度，并将结果赋值给 `length` 变量。运行程序，我们可以得到与上一个例子相同的结果。

## 深入探讨

背后的原理是什么呢？其实，C ++中的字符串是以null结尾的字符数组，即最后一个元素的值为null (即 `'\0'`)。`strlen` 函数会从字符串的第一个元素开始遍历，直到遇到null，然后返回此时的遍历次数，即字符串的长度。

而 `string` 类中的 `length` 方法则是返回字符串中字符的个数，不包括最后的null。因此，如果字符串中没有null，那么 `strlen` 和 `length` 所返回的结果将是一样的。

## 参考资料

- [C ++中的字符串](https://www.geeksforgeeks.org/c-plus-plus/)
- [C ++标准库中的字符串类(string)](https://www.cplusplus.com/reference/string/)

## 链接

- [C ++中的strlen函数](https://www.geeksforgeeks.org/strlen-in-c/)
- [C ++中的字符串前奏](https://www.w3schools.com/cpp/cpp_strings.asp)
- [C ++中的字符串函数及其用法](https://www.tutorialspoint.com/cplusplus/cpp_strings.htm)