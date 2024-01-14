---
title:                "Arduino: 提取子字符串"
simple_title:         "提取子字符串"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

# 为什么

在编程中，经常会遇到需要从一个字符串中提取特定的部分的情况。这可以帮助我们更有效地处理数据和优化程序。在 Arduino 编程中，提取子字符串也是一个常见的需求，这是为什么学习这个技术是非常重要的。

# 如何做

在 Arduino 中，提取子字符串可以通过使用 `substring()` 函数来实现。这个函数需要两个参数：起始索引和结束索引。下面是一个示例代码，展示如何提取一个字符串中的子字符串并输出结果。

```
Arduino String str = "Hello World!";

String newStr = str.substring(0, 5);

Serial.println(newStr); // 输出 "Hello"
```

上面的代码中，我们定义了一个字符串 `str`，然后使用 `substring()` 函数提取了索引为 0 到 5 的子字符串，并将结果赋值给 `newStr`。最后，我们使用 `Serial` 对象打印了新的子字符串。

另外，我们也可以使用 `indexOf()` 函数来找到子字符串的起始索引，然后再将它作为 `substring()` 函数的参数。下面是一个示例代码，展示如何从一个较长的字符串中提取一个单词并输出结果。

```
Arduino String sentence = "I love coding with Arduino!";

int loveIndex = sentence.indexOf("love"); // 找到 "love" 单词的起始索引

String love = sentence.substring(loveIndex, loveIndex+4); // 提取 "love" 单词并赋值给新的字符串变量

Serial.println(love); // 输出 "love"
```

# 深入了解

除了上面介绍的基本用法，还有一些其他方法可以用来提取子字符串，比如使用正则表达式、循环结构等。此外，我们还可以通过修改指针来实现提取子字符串的目的。这些方法需要更深入的了解和编程技巧，但是它们可以帮助我们更灵活地提取任何想要的子字符串。

# 查看更多

- 经典 Arduino 参考手册中的 [substring()](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/) 函数文档
- [C++ String 类参考手册](http://www.cplusplus.com/reference/string/string/substring/)
- [正则表达式入门教程](https://www.runoob.com/regexp/regexp-tutorial.html)