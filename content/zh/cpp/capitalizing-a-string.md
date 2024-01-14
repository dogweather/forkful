---
title:    "C++: 把字符串转成大写"
keywords: ["C++"]
---

{{< edit_this_page >}}

为什么：为什么会有人对C++中的字符串进行大写处理。

如何：在下面的“```C++ ...```”代码块中，提供了编码示例和示例输出。

## 为什么
大写处理字符串在编程中是一个常见的需求。它可以帮助我们规范输入的数据，使得比较更加准确，也可以提升视觉效果，让程序更加美观。

## 如何
首先，我们需要定义一个字符串变量并赋值：
```C++
string str = "hello world";
```
接下来，我们需要使用一个循环来遍历字符串中的每个字符，并将其转换为大写形式：
```C++
for (int i = 0; i < str.length(); i++) {
    str[i] = toupper(str[i]);
}
```
最后，我们可以输出处理后的结果：
```C++
cout << str << endl; // 输出：HELLO WORLD
```

## 深入了解
在C++中，字符串被表示为字符的数组。因此，我们可以使用循环来遍历数组并对每个字符进行处理。上面的例子中，我们使用了`toupper()`函数来将字符转换为大写形式。该函数属于C++标准库中的`<cctype>`头文件，我们需要在代码中进行包含`#include <cctype>`，才能使用这个函数。

## 参考资料
- [C++字符串大小写转换](https://blog.csdn.net/IT_Leader_W/article/details/81377162)
- [C++标准库头文件cctype](https://zh.cppreference.com/w/cpp/header/cctype)

## 参考资料
- [C++字符串大小写转换](https://blog.csdn.net/IT_Leader_W/article/details/81377162)
- [C++标准库头文件cctype](https://zh.cppreference.com/w/cpp/header/cctype)