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

## 什么是提取子字符串？
提取子字符串就是从一个字符串中截取一个小段的字符，比如从"Hello World!"中提取"World"。程序员通常会使用它来处理字符串数据，例如从传感器读取的数据中提取特定的信息。

## 如何提取子字符串：
```
Arduino code block
// 假设有以下字符串：
String data = "Sensor ID: 123; Temperature: 25℃";
// 我们想提取出传感器ID和温度值，可以按照以下步骤进行操作：
// 1. 首先，用indexOf()函数找到分号的位置，将其存储在一个变量中
int semicolon = data.indexOf(';');
// 2. 然后，使用substring()函数提取分号之前的字符串
String sensorID = data.substring(11, semicolon);
// 3. 接着，用lastIndexOf()函数找出℃符号的位置
int degree = data.lastIndexOf('℃');
// 4. 最后，使用substring()函数提取℃符号之前的字符串
String temperature = data.substring(27, degree);
```
输出结果：
```
Sensor ID: 123
Temperature: 25
```

## 深入了解：
提取子字符串的概念并非特定于Arduino，它在编程中也非常常见。除了用于提取特定的信息，再将它们整合起来作为一个新的字符串之外，我们也可以使用其他方法来达到同样的目的，例如使用正则表达式或者分割字符串。Arduino中的substring()函数是基于Java语言的，它允许我们指定起始和结束位置来提取字符串，但在其他编程语言中可能会有不同的用法。

## 相关链接：
- [Arduino文档中的substring()函数](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/ "Arduino substring()函数文档")
- [Java文档中的substring()函数](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#substring-int-int "Java substring()函数文档")
- [关于正则表达式的相关资源](https://www.regular-expressions.info/ "正则表达式资源")