---
title:                "Arduino: 将字符串大写"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

为什么：为什么要进行字符串的大写化？一般来说，对于一些文本信息的处理和展示，有时需要将字符串的首字母大写来提升可读性和美观性。

## 如何实现

```Arduino
// 定义字符串变量
String myString = "hello world";

// 使用 built-in 函数 toUpperCase() 将字符串的首字母大写
// 并将结果赋值给新变量
String myNewString = myString.toUpperCase();

// 输出结果
Serial.println(myNewString);
```

输出：Hello world

## 深入了解

字符串的大写化实际上是将 ASCII 码表中的小写字母转换为对应的大写字母。Arduino 提供了 `toUpperCase()` 函数来实现这一转换，同时也可以通过编写自定义函数来达到同样的效果。同时，需要注意的是，`toUpperCase()` 函数只会将字符串的首字母转换为大写，并不会影响字符串中其他位置的大小写。

## 参考资料

[ASCII table](https://www.arduino.cc/en/Reference/ASCIITable)

[Arduino Language Reference - String Functions](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/touppercase/)

## 查看更多

[Markdown 中文版教程](https://www.appinn.com/markdown/)