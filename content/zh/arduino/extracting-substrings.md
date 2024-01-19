---
title:                "提取子字符串"
html_title:           "Arduino: 提取子字符串"
simple_title:         "提取子字符串"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## 什么和为什么?

子字符串抽取是在字符串中选择特定部分的过程。程序员这样做是为了解析、操作或理解字符串的部分内容。

## 如何操作:

以下是如何在Arduino程序中抽取子字符串的例子：

```Arduino
String str = "Hello, Arduino!";
String subStr = str.substring(7, 14);
Serial.begin(9600);
Serial.println(subStr);
```

运行此程序，串口监视器将会打印出:

```Arduino
Arduino
```

在这里，我们从下标7到13，抽取出了子字符串 "Arduino"。

## 深入研究:

 - 历史背景：子字符串抽取技术的概念可以追溯到计算机编程的初期，那时处理文字信息变得重要起来。

 - 替代方案：除了 `substring()`，你可以使用诸如 `charAt()` 这样直接地址字符的方法，但你需要更加的努力来实现抽取。

 - 实现细节： `substring()` 返回一个新的字符串对象，这意味着它会占用额外的内存。针对大量数据时，需要考虑内存管理。

## 参考资料:

 - [Arduino String Objects](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/) - 官方Arduino库对字符串对象的详小描述。
 
 - [String substring()](https://www.arduino.cc/en/Tutorial/StringSubstring) - 官方Arduino教程详尽描述了如何在程序中抽取子字符串。