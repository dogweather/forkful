---
title:    "Arduino: 将字符串转换为大写字母"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

为什么：首先，让我们来看看为什么我们需要学习如何使用Arduino编程去大写字符串。大写字符串是一种非常有用的技能，它可以帮助我们在代码中处理字符串，使它们更加容易识别和处理。

## 如何操作

要开始大写字符串，我们需要首先引入Arduino的字符串库。这可以通过在sketch中添加以下代码来实现： 

```Arduino
#include <String.h>
```

接下来，我们需要创建一个字符串变量，并给它赋予我们想要大写的字符串。假设我们想要大写"hello"，那么我们可以这样写： 

```Arduino
String myString = "hello";
```

接下来，我们需要使用`toUpperCase()`函数来大写我们的字符串。我们可以这样写： 

```Arduino
myString.toUpperCase();
```

最后，我们可以使用`println()`函数来输出大写的字符串并验证结果。整个代码如下所示： 

```Arduino
#include <String.h>

String myString = "hello";
myString.toUpperCase();
println(myString);
```

运行代码后，我们可以在串口监视器中看到输出结果为"HELLO"，这就是我们所期待的结果！

## 深入探讨

现在，让我们来更深入地了解一下如何大写字符串。`toUpperCase()`函数实际上是使用ASCII码来改变字符串中的每个字符的值。ASCII码是一种常用的字符编码系统，它为每个字符分配一个唯一的数字值。大写字母比小写字母的ASCII码值小32。因此，通过加上32，我们可以将小写字母转换为大写字母。

除了使用`toUpperCase()`函数，我们还可以通过循环遍历每个字符，并使用`toUpper()`函数来逐个改变字符的值。这可以帮助我们更深入地理解字符串的处理过程。

## 参考链接

- [Arduino官方文档-字符串库](https://www.arduino.cc/reference/zh/libraries/string/)
- [ASCII码表](http://www.asciitable.com/)