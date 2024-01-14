---
title:                "Arduino: 连接字符串"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## 为什么

在程序设计中，字符串（string）是一个非常常用的数据类型。而串联字符串（concatenating strings）则是将多个字符串连接起来形成一个新的字符串的操作。这在编程中可以用于创建用户友好的输出语句，也可以用于将多个变量或文本组合在一起进行处理。所以，掌握字符串的串联方法是非常重要的。

## 如何

串联字符串在Arduino编程中可以通过两种方法实现：使用字符串拼接函数`strcat()`和运算符`+`。下面是两种方法的示例代码和输出结果：

```Arduino
// 使用strcat()函数
char str1[10] = "Hello";
char str2[10] = "World";
strcat(str1, str2);
Serial.println(str1); // 输出结果为"HelloWorld"

// 使用运算符+
String string1 = "Good";
String string2 = "Morning";
String string3 = string1 + string2;
Serial.println(string3); // 输出结果为"GoodMorning"
```

## 深入探讨

在上面的示例中，我们使用了两种不同的数据类型来实现字符串的串联。`strcat()`函数适用于字符数组，而运算符`+`适用于字符串对象。同时，对于字符数组使用`strcat()`函数，我们需要先确定数组大小能够容纳新的字符串，否则可能会导致内存溢出的问题。

此外，Arduino的`String`类有一个特殊的方法`concat()`，其功能和`strcat()`函数类似。你可以尝试使用这个方法来实现字符串的串联，并比较其和`+`运算符的差异。

## 参考链接

- [Arduino官方文档：strcat()函数](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/strcat/)
- [Arduino官方文档：String类](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [Arduino Playground：StringsConcatenation](https://playground.arduino.cc/Code/StringsConcatenation/)