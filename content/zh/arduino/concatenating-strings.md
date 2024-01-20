---
title:                "连接字符串"
html_title:           "C: 连接字符串"
simple_title:         "连接字符串"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

Title: 字符串连接在Arduino编程中的使用 (Use of String Concatenation in Arduino Programming)

## 何为何所以?
连接字符串是将两个或多个字符串连在一起形成一个新的更长的字符串的过程。程序员之所以进行字符串连接，是因为它方便储存、传递或显示组合的信息。

## 手把手教程：
在Arduino编程中，有多种方法可以实现字符串的连接。以下是其中一些例子。

```Arduino
String str1 = "Hello,";
String str2 = " World!";
String str3 = str1 + str2;    // 使用 "+" 运算符来连接两个字符串
Serial.println(str3);          // 输出：Hello, World!
```

还可以使用 `concat()` 函数来连接字符串。

```Arduino
String str1 = "Hello,";
String str2 = " World!";
str1.concat(str2);            // 使用 concat() 函数来连接两个字符串
Serial.println(str1);         // 输出：Hello, World!
```

## 深度剖析:
连接字符串在计算机编程历史中有很长的历史，因为它是处理文本数据最基本的方法之一。然而，尽管连接在所有编程语言中都有，但使用的方法却不尽相同。

比如，C++中连接字符串的方法与Arduino中的方法有所不同，主要原因是Arduino使用的是缩小版的C++，有一些特别的库和操作被舍去以减小Arduino的运行和内存负担。

此外，关于字符串连接，还有一个重要的执行细节需要注意。那就是，连接字符串会消耗更多的RAM（随机存取存储器）。因为在Arduino中，每使用"+="、"+"或`concat()`函数来连接字符串，就会创建一个新的字符串，这会消耗更多的内存。所以，为了效率，程序员需要权衡在连接字符串和节省内存之间的取舍。