---
title:                "将字符串大写"
html_title:           "Arduino: 将字符串大写"
simple_title:         "将字符串大写"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 什么及为何?
当我们编写代码的时候，有时候需要将字符串中的每个单词的首字母大写。这样做的原因可能是为了让字符串更容易阅读和理解，或者是为了满足特定的格式要求。无论是哪种情况，对字符串进行大写处理都是一项常见的任务。

## 如何:
下面是一个使用Arduino代码当中capitalize函数将字符串首字母大写的简单示例:
```
char str[] = "hello world";
capitalize(str);
//输出: "Hello World"
```

## 深入了解:
在计算机编程历史上，字符串大写处理是一项非常重要的功能。在早期的编程语言中，如C和Pascal，大写处理是一个必备的功能，因为它们无法处理不同大小写的单词。然而，在现代编程语言中，这项功能已经被整合到字符串处理方法中，使得更加简单和方便。

除了capitalize函数，我们还可以使用其他方法来实现字符串大写处理，比如循环遍历字符串，逐个判断每个字符是否为小写，然后转换为对应的大写字符。

## 参考链接:
- [Arduino参考资料](https://www.arduino.cc/reference/en/language/functions/communication/serial/print/)
- [字符串大写处理方法](https://www.w3schools.com/cpp/cpp_strings_uppercase.asp)