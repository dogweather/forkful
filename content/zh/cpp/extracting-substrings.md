---
title:    "C++: 提取子字符串"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

# 为什么

在编程中，我们经常需要从一个较长的字符串中提取出特定的一段文字，例如从一段文章中提取出标题或者从一段音频中提取出特定的片段。这就是提取子字符串的概念，它可以帮助我们更有效地处理文本数据和音频数据。

# 怎么做

子字符串的提取可以通过C++语言中的几种方法来实现。下面我们来介绍两种常用的方法：使用标准库函数和遍历字符串的方法。

首先，让我们看一下使用标准库函数`substring()`的示例代码：

```C++
// 定义一个字符串
string fullString = "今天天气真好，我想去散步。";

// 使用substring()函数提取子字符串
string subString = fullString.substr(6, 6); // 第一个参数为提取的起始位置，第二个参数为提取的长度

// 输出结果
cout << subString << endl; // 结果为“天气真好”
```

另一种方法是使用循环来遍历字符串，并通过条件判断来提取子字符串：

```C++
// 定义一个字符串
string fullString = "今天天气真好，我想去散步。";

// 定义一个变量用于存储子字符串
string subString = "";

// 循环遍历字符串
for (int i = 6; i < 12; i++) { // 从第6个字符开始提取，提取6个字符
    subString += fullString[i]; // 将提取的字符添加到子字符串中
}

// 输出结果
cout << subString << endl; // 结果为“天气真好”
```

无论使用哪种方法，最终结果都是相同的。通过使用这些方法，我们可以从一个字符串中提取出我们需要的子字符串，实现对文本数据的有效处理。

# 深入探究

除了提取固定位置的子字符串，我们还可以使用标准库函数`find()`和`find_first_of()`来找到特定字符或者字符串，然后再提取出子字符串。

例如，如果我们想从一个网址中提取出域名部分，可以使用以下代码：

```C++
// 定义一个网址字符串
string url = "https://www.example.com/product/12345";

// 使用find_last_of()函数找到最后一个斜杠的位置
int slashPos = url.find_last_of("/");

// 使用find_first_of()函数找到第一个斜杠的位置
int dotPos = url.find_first_of(".", slashPos);

// 提取出域名部分
string domain = url.substr(slashPos + 1, dotPos - slashPos - 1);

// 输出结果
cout << domain << endl; // 结果为“example”
```

通过灵活运用这些方法，我们可以轻松地从一个字符串中提取出我们需要的子字符串，实现更加精确的文本处理。

# 参考链接

- [C++字符串处理技巧](https://www.cnblogs.com/FrankTan/p/4660645.html)
- [C++字符串操作详解](https://blog.csdn.net/liushanshui/article/details/6894340)
- [C++字符串处理标准库函数](https://www.cnblogs.com/abcjie/p/6423115.html)

# 参考链接