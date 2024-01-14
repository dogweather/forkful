---
title:                "Arduino: 字符串连接"
simple_title:         "字符串连接"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## 为什么

为什么要学习连接字符串？如果你想要在Arduino编程中创建更多的自定义输出，连接字符串是一个必不可少的技巧。通过连接字符串，你可以将多个变量或数据片段组合成一个更长的字符串，从而实现更灵活的输出。

## 如何

连接字符串在Arduino编程中非常简单。首先，你需要声明一个字符串变量并赋值一个初始字符串，例如：

```arduino
String myString = "Hello, ";
```

接下来，你可以使用“+”符号来连接其他字符串或变量，例如：

```arduino
myString = myString + "world!";
```

最终，myString的值将会是 "Hello, world!"。你也可以连接多个字符串：

```arduino
myString = "My favorite animal is a " + "panda.";
```

最终，myString的值将会是 "My favorite animal is a panda."。你也可以连接数字变量，例如：

```arduino
int num = 5;
myString = "I have " + String(num) + " cats.";
```

最终，myString的值将会是 "I have 5 cats."。你也可以在连接字符串时使用变量或数字的方式来格式化输出，例如：

```arduino
int num = 10;
myString = "I have " + String(num, DEC) + " dollars.";
```

最终，myString的值将会是"I have 10 dollars."。在连接字符串时，你也可以使用其他常见的字符串操作函数，例如：

```arduino
/* 替换字符串中的部分内容 */
String replacedString = myString.replace("panda", "unicorn");

/* 删除字符串首尾的空格 */
String trimmedString = myString.trim();

/* 将字符串全部转换为小写 */
String lowercaseString = myString.toLowerCase();

/* 将字符串全部转换为大写 */
String uppercaseString = myString.toUpperCase();
```

## 深入探讨

在Arduino编程中，使用连接字符串可以帮助你实现更复杂的输出和控制。你可以使用连接字符串来构建复杂的消息、数据日志和用户界面。此外，连接字符串也可以与其他数据类型的变量和逻辑运算符一起使用，从而实现更多的功能。

请注意，连接字符串是一种方便的技巧，但过多的使用可能会导致代码变得复杂。因此，务必谨慎使用连接字符串，始终保持代码的简洁性和可读性。

## 查看更多示例

- [Arduino - String 类型](https://www.arduino.cc/reference/zh/language/variables/data-types/string/)
- [Arduino - 字符串操作函数](https://www.arduino.cc/reference/zh/language/variables/data-types/string/functions/)
- [Arduino String 示例](https://www.arduino.cc/en/Tutorial/StringAppendOperatorExample)