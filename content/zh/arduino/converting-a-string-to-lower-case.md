---
title:                "Arduino: 将字符串转换为小写字母"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

低级转换 大小 In this blog post, we will be exploring the process of converting a string to lower case in Arduino programming. Whether you are just starting out with Arduino or are looking to expand your programming skills, understanding how to convert a string to lower case can be a useful tool in your coding arsenal. 完整过程

 首先，让我们来看一下为什么要进行字符串转换为小写。在编程中，字符串是一系列字符的序列，有时候我们需要对字符串进行格式化，使其符合特定的条件。一种常见的需求是将字符串转换为小写，这可以让我们更容易地进行比较和处理字符串。

怎么做

Arduino编程语言有许多内置函数，可以帮助我们实现字符串转换为小写的功能。其中一个函数是toLowerCase（），它可以将字符串中的所有字符转换为小写。让我们来看一个简单的例子：

```
Arduino String str = "Hello World!";
str = str.toLowerCase();
Serial.println(str); // output: hello world!
```

在这个例子中，我们首先创建一个字符串变量，并将其赋值为“Hello World!”。然后，我们使用toLowerCase（）函数将字符串转换为小写，并将其重新赋值给原来的变量。最后，我们使用Serial.println（）将转换后的字符串输出到串行监视器中，结果会显示为小写。

深入探讨

嗯，现在我们已经知道了如何在Arduino中进行字符串转换为小写，但是你可能会想知道这个函数的工作原理是什么。事实上，在字符串转换为小写的过程中，toLowerCase（）函数会遍历字符串中的每一个字符，并针对每个字符使用tolower（）函数进行转换。tolower（）函数将大写字母转换为小写字母，而对于已经是小写的字母则不做处理。这样，就能实现整个字符串的转换为小写。

另外值得一提的是，toLowerCase（）函数只对字母字符有效，对于数字、特殊字符或空格则不会做任何转换。这意味着在对字符串进行转换前，我们需要先确保字符串中只包含字母字符。

参考链接

- 将字符串转换为小写: https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/tolowercase/

- toLowerCase（）函数文档: https://www.arduino.cc/reference/en/language/functions/character-functions/tolower/

- 字符串概述: https://www.arduino.cc/reference/en/language/variables/data-types/string/

查看更多

希望这篇文章能帮助你学习如何在Arduino中进行字符串转换为小写。如果你想了解更多关于字符串的知识，可以参考上面提供的参考链接。

也别忘了浏览我们的其他博客文章，帮助你更深入地了解Arduino编程。