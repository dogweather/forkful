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

# 究竟是什么？
字符串插值指的是在字符串中插入变量或表达式，以便在输出中显示更复杂的文本。程序员这样做是为了更有效地构建文本输出，而无需手动拼接字符串或使用大量的 if/else 语句。

# 如何操作：
在 Arduino 中，我们可以使用 str = "text {}" 的格式来定义一个字符串，其中 {} 表示要插入的变量或表达式。然后使用 ```Arduino Serial.println(str)``` 命令来打印出带有插值的字符串。例如，如果我们定义了一个串口变量 int num = 5，并将 num 插入到字符串 "我现在有 {} 个苹果" 中，那么输出将是 "我现在有 5 个苹果"。

# 深入挖掘：
字符串插值最早出现在 Perl 编程语言中，它的目的是为了简化字符串拼接的过程。在其他编程语言中，也有类似的实现，如 C# 中的 String.Format() 方法。在 Arduino 中，还可以通过使用 sprintf() 函数来实现字符串插值。

# 参考链接：
- https://zhuanlan.zhihu.com/p/30271065
- https://en.wikipedia.org/wiki/String_interpolation
- https://www.arduino.cc/reference/en/language/functions/communication/serial/println/
- https://www.arduino.cc/reference/en/language/functions/communication/serial/sprintf/