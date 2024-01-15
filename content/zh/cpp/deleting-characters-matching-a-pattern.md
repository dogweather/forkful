---
title:                "删除匹配模式的字符"
html_title:           "C++: 删除匹配模式的字符"
simple_title:         "删除匹配模式的字符"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 为什么

有时候，我们可能需要从字符串中删除符合特定模式的字符。这可能是因为我们想要对字符串进行清理，或者从文本中删除敏感或无效的字符。使用C++编程语言，我们可以轻松地删除字符串中匹配特定模式的字符。

## 如何操作

使用C++编程语言删除字符串中匹配特定模式的字符非常简单。我们只需要遵循以下三个步骤：
1. 定义一个用于存储字符串的变量。
2. 使用C++的内置函数或自定义函数，在变量中查找并删除符合特定模式的字符。
3. 打印输出结果或将结果存储在新的变量中。

让我们来看一个简单的实例，在字符串中删除所有的数字。在下面的C++代码示例中，我们将使用字符串变量“str”来存储字符串，使用C++的for循环和isnumeric()函数来遍历并删除字符串中的所有数字，最后将结果存储在新的变量“new_str”中。

```C++
#include <iostream>
#include <string>
using namespace std;

int main() {
  string str = "ABC123!@#";
  string new_str = "";

  for (int i = 0; i < str.size(); i++) {
    if (!isnumeric(str[i])) {
      new_str += str[i];
    }
  }

  cout << "原字符串： " << str << endl;
  cout << "删除数字后的字符串： " << new_str;

  return 0;
}
```

输出结果为：
```
原字符串： ABC123!@#
删除数字后的字符串： ABC!@#
```

## 深入了解

在C++中，我们可以使用不同的方法来删除字符串中匹配特定模式的字符。例如，我们可以使用C++的replace()函数来替换字符串中的特定字符，或者使用erase()函数来删除字符串中的特定字符位置。此外，我们还可以使用自定义函数来实现更复杂的匹配模式和删除操作。记住，在使用自定义函数时，需要在程序中包含函数的原型声明。

## 参考链接

- [C++字符串处理的基本操作](https://www.zhihu.com/question/52974618)
- [C++标准库函数列表](http://c.biancheng.net/cpp/u/cpp_function_list.html)
- [C++中的字符串处理函数](https://www.runoob.com/w3cnote/cpp-string-functions.html)

**相关文章：**

## 参见

- ["C++字符串处理：删除特定字符"](https://github.com/example/delete-matching-characters-cpp/blob/master/delete-matching-characters-cpp.md)
- ["C++字符串处理：替换特定字符"](https://github.com/example/replace-specific-character-cpp/blob/master/replace-specific-character-cpp.md)