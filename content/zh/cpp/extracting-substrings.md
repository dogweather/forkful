---
title:                "提取子字符串"
html_title:           "C++: 提取子字符串"
simple_title:         "提取子字符串"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## 为什么

在编程过程中，经常会遇到需要从一个字符串中提取其中的一部分内容的情况。这个过程就被称为提取子字符串。提取子字符串可以帮助我们更有效地处理文本数据，使得我们的程序更加灵活和可靠。

## 如何做

提取子字符串有很多种方法，下面将介绍其中几种常用的方法。在每种方法的示例后面，都会附上相应的代码块和运行结果，方便读者更好地理解和实践。

### 1. 使用 `substr()` 方法提取子字符串

首先，我们需要包含 `string` 头文件，因为 `substr()` 方法是由该头文件所提供的。

```C++
// 包含string头文件
#include <string>

int main() {
  // 定义一个字符串
  std::string str = "Hello world!";

  // 使用substr()方法提取子字符串
  std::string substr = str.substr(6, 5);  // 提取从第六个字符开始的五个字符

  // 打印提取的子字符串
  std::cout << substr << std::endl;

  return 0;
}
```

运行结果为：

```
world
```

### 2. 使用 `substr()` 方法提取到字符串末尾

除了指定子字符串的起始位置和长度外，还可以使用 `std::string::npos` 来表示提取到字符串的末尾。例如：

```C++
// 包含string头文件
#include <string>

int main() {
  // 定义一个字符串
  std::string str = "Hello world!";

  // 使用substr()方法提取子字符串
  std::string substr = str.substr(6);  // 从第6个字符开始，一直提取到末尾

  // 打印提取的子字符串
  std::cout << substr << std::endl;

  return 0;
}
```

运行结果为：

```
world!
```

### 3. 使用 `find()` 方法提取子字符串

除了使用起始位置和长度来提取子字符串外，还可以使用 `find()` 方法来找到子字符串的起始位置。然后再利用 `substr()` 方法提取子字符串。

```C++
// 包含string头文件
#include <string>

int main() {
  // 定义一个字符串
  std::string str = "Hello world!";

  // 使用find()方法找到子字符串的起始位置
  int start_pos = str.find("world");

  // 使用substr()方法提取子字符串
  std::string substr = str.substr(start_pos, 5);  // 从起始位置开始提取五个字符

  // 打印提取的子字符串
  std::cout << substr << std::endl;

  return 0;
}
```

运行结果为：

```
world
```

## 深入了解

除了上面介绍的方法外，提取子字符串还有其他的一些技巧。比如可以使用 `replace()` 方法来替换子字符串，再利用 `istringstream` 类来将字符串拆分为子字符串。同时，还可以使用正则表达式来提取特定格式的子字符串。这些方法都可以帮助我们更加灵活地提取子字符串。

## 参考资料

- [C++ String manipulation](https://www.cplusplus.com/reference/string/)
- [Online regex tester and debugger](https://regex101.com/)

## 参见

- [C++ Strings and Substrings](https://www.programiz.com/cpp-programming/library-function/string/substr)
- [C++ Standard Library](https://en.cppreference.com/w/cpp/string/basic_string)