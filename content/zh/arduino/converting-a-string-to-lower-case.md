---
title:                "Arduino: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 为什么

*为什么会需要将字符串转换为小写？*

在Arduino编程中，经常需要处理用户输入的字符串。有时候，为了保证数据的一致性和准确性，需要将字符串转换为统一的大小写格式。例如，将用户输入的用户名转换为小写后，可以避免用户输入大小写不一致导致的登录问题。

## 如何操作

在Arduino中，可以使用自带的toLowerCase()函数来实现字符串转换为小写。下面是一个简单的例子，演示如何使用这个函数：

```Arduino
String str = "Hello World";
str.toLowerCase(); // 将字符串转换为小写
Serial.println(str);  // 输出结果：hello world
```

在上面的代码中，我们首先创建一个字符串`str`，内容为"Hello World"。然后，调用`toLowerCase()`函数将其转换为小写，并使用串口打印函数`Serial.println()`输出结果。你也可以将这个函数的返回值赋给一个新的字符串变量，这样原始字符串的内容不会改变。

## 深入了解

`toLowerCase()`函数是针对整个字符串操作的，它会将所有的字符都转换为小写形式。如果你只想转换字符串中的某些字符，可以使用下标来逐个修改。例如：

```Arduino
String str = "Hello World";
for (int i = 0; i < str.length(); i++) {
  if (str[i] >= 'A' && str[i] <= 'Z') { // 如果字符为大写字母
    str[i] = str[i] + 32; // 使用ASCII码将其转换为小写
  }
}
Serial.println(str); // 输出结果：hello world
```

除了`toLowerCase()`函数，Arduino还提供了其他的字符串函数，如`toUpperCase()`用于转换为大写，`substring()`用于截取子字符串等，可以根据实际需要选择合适的函数来处理字符串。

## 另请参阅

1. [Arduino官方文档](https://www.arduino.cc/reference/en/language/functions/strings/stringtolowercase/)
2. [ASCII码表](https://www.asciitable.com/)