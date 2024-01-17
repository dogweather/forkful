---
title:                "读取命令行参数"
html_title:           "Arduino: 读取命令行参数"
simple_title:         "读取命令行参数"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# 什么是命令行参数？为什么程序员需要读取它们？
命令行参数是在运行程序时通过命令行输入的参数，可以影响程序的执行。程序员读取命令行参数可以使程序更灵活，可以根据输入的参数来执行不同的功能。

# 如何读取命令行参数？
可以使用Arduino的Serial库中的readStringUntil()函数来读取命令行参数，该函数可以指定一个特定的字符作为输入的结束标志。例如，下面的代码段可以读取用户输入的参数，当检测到“#”字符时结束，并将参数打印出来：
```Arduino
String argument = Serial.readStringUntil('#');
Serial.println(argument);
```
如果用户在输入参数后没有输入“#”字符，那么程序将会一直等待用户输入。输入的参数可以通过Arduino的串口进行实时监测和调试。

# 深入了解
命令行参数的概念起源于早期的命令行界面，而在图形界面成为主流之后，它仍然被广泛地使用。除了使用Serial库读取命令行参数外，还可以使用其他库如SoftwareSerial来实现串口通信。此外，一些操作系统和编程语言也提供了读取命令行参数的内置函数，比如C语言中的main函数的参数。

# 参考链接
- [Serial.readStringUnitil()参考文档](https://www.arduino.cc/reference/en/language/functions/communication/serial/readstringuntil/)
- [SoftwareSerial库参考文档](https://www.arduino.cc/en/Reference/SoftwareSerial)
- [C语言中main函数的参数](https://www.tutorialspoint.com/cprogramming/c_command_line_arguments.htm)