---
title:                "连接字符串"
html_title:           "Arduino: 连接字符串"
simple_title:         "连接字符串"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

#为什么#

#为什么人们会想要连接字符串#

连接字符串是将多个字符串合并为一个更长的字符串的过程。这在编程中非常有用，可以帮助我们组织并展示特定信息，例如在Arduino项目中显示传感器数据或创建个性化消息。如果您想要创建一个功能强大而有趣的项目，那么了解如何连接字符串是很重要的。

#如何操作#

要在Arduino中连接字符串，您需要使用字符串对象（String objects）和concat()函数。以下是一个简单的例子，该例子需要两个字符串并将它们连接起来。

```Arduino
String message1 = "Hello";
String message2 = "world!";
String combined = message1.concat(message2);
Serial.println(combined);
```

您会在串行监视器中看到打印输出“Hello world!”。concat()函数将第一个字符串和第二个字符串连接起来，并将结果存储在combined变量中。您还可以连接多个字符串，只需要连续使用多次concat()函数即可。

```Arduino
String message1 = "Welcome ";
String message2 = "to ";
String message3 = "my ";
String message4 = "Arduino project!";
String combined = message1.concat(message2).concat(message3).concat(message4);
Serial.println(combined);
```

这次，输出将是“Welcome to my Arduino project!”。正如您所见，您可以使用concat()函数将任意数量的字符串连接在一起，只要您在每个concat()函数后面添加一个点（.）。

#深入了解#

连接字符串时需要注意的一个问题是性能。在Arduino项目中，内存空间有限，因此为了保持最佳性能，建议避免使用字符串对象和concat()函数。相反，您可以使用C语言中的指针来连接字符串。

以下是一个示例代码，展示如何使用指针来连接字符串。

```Arduino
char message1[] = "Hello";
char message2[] = "world!";
char *combined;

combined = strcat(message1, message2);
Serial.println(combined);
```

输出将是“Helloworld!”。在这个例子中，我们使用strcat()函数（在C语言中用于连接字符串）和指针来提高性能。这是因为指针直接操作内存地址，而不是创建新的字符串对象和变量。

#参考链接#

[Arduino文档: String](https://www.arduino.cc/reference/en/language/variables/data-types/string/)

[C语言资料: String Concatenation](https://www.tutorialspoint.com/c_standard_library/c_function_strcat.htm)

#相关阅读#