---
title:    "C++: 使用正则表达式"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

为什么：为什么要使用正则表达式?

正则表达式是一种非常强大的工具，它可以帮助我们在文本中快速地匹配和提取特定的模式。它可以在字符串处理、文本解析、模式匹配和数据验证等方面发挥巨大作用。使用正则表达式可以大大提高我们编程的效率和准确性。

## 如何使用

为了使用正则表达式，我们首先需要包含`<regex>`头文件。然后，我们可以使用`std::regex`类来创建一个正则表达式对象，并将要匹配的模式作为参数传递给它的构造函数。

```C++
#include <regex>
using namespace std;

// 创建一个名为pattern的正则表达式对象，用于匹配数字
regex pattern("[0-9]+");
```

接下来，我们可以使用`std::regex_match()`函数来对字符串进行匹配。该函数接受两个参数：要匹配的字符串和正则表达式对象。如果匹配成功，该函数会返回一个`std::smatch`对象，其中包含了匹配结果的相关信息。

```C++
string str = "12345";
// 使用正则表达式对象pattern匹配字符串str
smatch match;
regex_match(str, match, pattern);

// 输出匹配结果
cout << match.str(); // output: 12345
```

除了`std::regex_match()`函数外，还有一些其他函数可以用于正则表达式的匹配，例如`std::regex_search()`和`std::regex_replace()`等。通过这些函数，我们可以更加灵活地处理字符串，并且提取出我们需要的信息。

```C++
string str = "My email is abc123@test.com.";
regex pattern("[a-z0-9]+@[a-z]+\\.[a-z]+");
smatch match;

// 使用正则表达式对象pattern进行字符串搜索
if (regex_search(str, match, pattern)) {
    // 通过迭代器访问搜索到的结果
    for (auto it = match.begin(); it != match.end(); it++) {
        cout << *it << endl; // output: abc123@test.com
    }
}

// 使用正则表达式对象pattern来替换字符串中的邮箱地址
string new_str = regex_replace(str, pattern, "newemail@test.com");
cout << new_str; // output: My email is newemail@test.com.
```

## 深入了解

正则表达式的语法非常复杂，包含了许多字符和特殊的用法。例如，通过使用`()`可以创建一个子表达式，并通过`|`来创建多个选择条件，使用`[]`来表示一个字符集合，使用`{m,n}`来限定重复次数等等。针对不同的匹配需求，我们可以使用不同的正则表达式来实现。

值得一提的是，C++11标准中引入了原始字符串字面量（Raw String Literal）的概念，可以帮助我们更加方便地书写正则表达式。通过在字符串前使用`R"()"`来标识原始字符串，我们可以省略掉对特殊字符的转义。

```C++
string str = "C:\\Users\\Desktop\\test.txt";
// 使用原始字符串字面量来书写正则表达式
regex pattern(R"(C:\\\w+\\Desktop\\\w+\.txt)");
```

总的来说，正则表达式是一项非常强大的技巧，通过它我们可以对文本进行更加高效准确的操作。在日常的编程工作中，我们可以灵活运用正则表达式来处理字符串，提高我们的编程效率。

## 参考链接

- [CppReference](https://en.cppreference.com/w/cpp/regex)
- [Regular-Expressions.info](https://www.regular-expressions.info/cpp.html)
- [GeeksforGeeks](https://www.geeksforgeeks.org/regular-expression-in-cpp/)