---
title:                "插值字符串"
html_title:           "Arduino: 插值字符串"
simple_title:         "插值字符串"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 什么以及为什么?

插值字符串是程序中将变量的值插入到字符串中的过程。程序员通常进行字符串插值以方便显示、打印或存储格式化的信息。

## 如何操作:

在Arduino中，你可以利用串口通信的`printf`函数来执行字符串插值。看看以下的示例：

```Arduino
char buffer[50];
int temperature = 22;
sprintf(buffer, "The current temperature is: %d", temperature);
Serial.println(buffer);
```

这段代码的输出将会是:

```
The current temperature is: 22
```

## 深度探讨:

字符串插值在许多编程语言中都被广泛使用。C语言是最早引入这种功能的语言之一，之后通过Arduino扩展了这一功能。使用字符串插值，你可以在一个字符串中嵌入多个变量，而且每个变量都可以使用不同的格式。

替代方案有拼接，即一个个手动把字符粘贴在一起，但这种方式在处理复杂的字符串时会变得非常麻烦。

在实现上，它是通过寻找 `%` 符号，然后把之后的字符作为替换规则，把对应的变量值替换进去。如果你想要对字符串进行更复杂的操作，可能需要使用更高级的字符串处理函数如 `strcat` 或 `strncat`。

## 参阅其他：

2. [C语言字符串处理](https://www.runoob.com/cprogramming/c-strings.html)