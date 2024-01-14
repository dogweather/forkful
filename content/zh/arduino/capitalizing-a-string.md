---
title:                "Arduino: 将字符串设定为大写"
simple_title:         "将字符串设定为大写"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

为什么：为什么一个人要参与将字符串大写化的实践？

在编程中，经常需要对字符串进行处理和转换。大写化字符串可以使得字符串更容易被处理和识别，也可以在某些情况下提高代码的可读性。

## 如何操作

在Arduino中，我们可以通过调用`toUpperCase()`函数来实现将字符串大写化的功能。这个函数会接收一个字符串变量，并返回一个大写化后的新字符串。

```Arduino
String str = "hello world";
String new_str = str.toUpperCase();

Serial.println(new_str); // Output: HELLO WORLD
```

值得注意的是，该函数会直接修改原始的字符串变量，因此在调用完成后，`str`的值也被改为大写化后的结果。如果需要保留原始的字符串变量，可以在调用函数时传入临时变量或者使用字符串拼接的方式。

```Arduino
String str = "hello world";
String temp = str; // 创建临时变量保存原始值
String new_str = temp.toUpperCase();

Serial.println(str); // Output: hello world
Serial.println(new_str); // Output: HELLO WORLD
```

## 深入探讨

除了`toUpperCase()`函数，Arduino还提供了一些其他方法来处理字符串。比如可以使用`charAt()`函数来获取字符串中某个位置上的字符，然后通过`toUpperCase()`函数将其转换为大写形式。

```Arduino
String str = "apple";
char first_letter = str.charAt(0); // 获取第一个字符'a'
String new_str = String(first_letter).toUpperCase(); // 将'a'转换为大写形式

Serial.println(new_str); // Output: A
```

此外，Arduino还提供了其他一些有用的字符串处理函数，包括`substring()`、`indexOf()`、`startsWith()`等，可以根据实际需求对字符串进行操作和转换。

## 查看相关文章

- [Arduino官方文档 - 字符串操作](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/)
- [How to Capitalize the First Letter of a String in Arduino](https://www.makerguides.com/capitalize-first-letter-string-arduino/)
- [How to Manipulate Strings in Arduino](https://www.electronicshub.org/manipulating-strings-arduino/)
- [字符串处理工具库 - Arduino StringMan](https://github.com/avishorp/TinyGPSPlus)