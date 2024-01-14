---
title:    "Arduino: 将字符串转换为小写"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## 为什么

为什么要将一个字符串转换为小写？这个操作在程序设计中非常常见，它可以帮助我们统一字符串的格式，使得数据处理更加准确和方便。

## 如何做

在Arduino中，实现字符串转换为小写非常简单。我们可以使用Arduino的内置函数`toLowerCase()`来实现。下面是一个例子：

```Arduino
String str = "TESt";
str.toLowerCase();
Serial.println(str); // 输出为 "test"
```

我们首先定义一个字符串变量，并赋值为"TESt"。然后使用`toLowerCase()`函数将其转换为小写。最后，我们使用串口监视器输出该变量，可以看到它已经成功转换为小写了。

## 深入探究

要深入理解字符串转换为小写的原理，我们需要了解ASCII编码。在ASCII编码表中，大写字母和小写字母的ASCII码是相差32的。因此，我们可以通过修改字符串中每个字符的ASCII码来将其转换为小写。Arduino的`toLowerCase()`函数就是基于这一原理实现的。

## 参考链接

- [Arduino官方文档：String类](https://www.arduino.cc/reference/en/language/variables/data-types/stringclass/)
- [ASCII编码表](https://www.asciitable.com/)