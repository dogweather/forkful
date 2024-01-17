---
title:                "将字符串转换成小写"
html_title:           "Arduino: 将字符串转换成小写"
simple_title:         "将字符串转换成小写"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 什么和为什么？
将字符串转换为小写是将字符串中的所有字母变为小写形式的过程。程序员这样做的原因是为了统一字符串的格式，方便后续的比较和处理。

## 如何：
使用Arduino编程语言，可以通过以下代码将字符串转换为小写，并将结果输出到串行监视器：
```Arduino
String str = "Hello World"; // 原字符串
str.toLowerCase(); // 转换为小写形式
Serial.println(str); // 输出结果："hello world"
```

## 深入探讨：
历史背景：在早期的计算机系统中，文本处理往往只能处理大写字符。随着发展，对文本处理的要求越来越严格，程序员需要将文本统一为小写形式，以便于比较和处理。

替代方案：除了使用内置函数，程序员也可以通过编写自定义函数来实现字符串转换为小写。

实现细节：在转换过程中，程序会遍历字符串中的每个字符，并将大写字母转换为小写字母。在执行转换操作后，原字符串的值也会发生改变。

## 查看更多：
了解更多关于字符串处理的方法：https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/tolowercase/