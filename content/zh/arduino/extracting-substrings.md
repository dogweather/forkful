---
title:                "提取字符串"
html_title:           "Arduino: 提取字符串"
simple_title:         "提取字符串"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## 为什么
如果你想在Arduino中处理文本数据，提取子字符串是非常有用的。比如，你可能想要从传感器读取的字符串中提取特定的数值或字符。

## 如何进行
提取子字符串最简单的方法是使用Arduino中的String对象的substring()函数。该函数需要两个参数，即要提取的起始位置和终止位置。下面是一个示例代码：

```Arduino
String data = "Temperature: 25.5 degrees Celsius";
String temp = data.substring(13, 17);  //从第13个位置开始提取，提取4个字符（即25.5）
Serial.println(temp);  //输出：25.5
```
你也可以使用length()函数来动态计算字符串的长度，这样就不需要手动指定终止位置。例如：

```Arduino
String data = "Input: 12345678";
int inputLength = data.length();  //获取字符长度，这里为11
String input = data.substring(7, inputLength);  //从第7个位置开始提取，一直到字符串尾部（共提取4个字符，即1234）
Serial.println(input);  //输出：1234
```

## 深入探讨
除了使用substring()函数，你还可以使用字符数组来提取子字符串。这种方法需要使用strcpy()函数来将子字符串复制到一个新的字符数组中。示例如下：

```Arduino
String data = "Date: 10/25/2021";
char date[11];  //定义一个具有足够空间存储子字符串的字符数组（长度为11，包含结尾的空字符）
data.toCharArray(date, 11);  //将String对象复制到字符数组中
```

## 参考链接
- [Arduino官方文档](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/)
- [Arduino官方示例](https://www.arduino.cc/en/Tutorial/StringSubstring)