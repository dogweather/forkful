---
title:                "Arduino: 阅读命令行参数。"
simple_title:         "阅读命令行参数。"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 为什么：为什么阅读命令行参数是重要的

在编程中，理解如何读取和使用命令行参数是非常重要的。命令行参数可以让我们在运行程序时输入自定义的参数，这样我们就可以实现更灵活的代码功能。对于Arduino编程来说，学习如何读取命令行参数会让我们的代码更加智能和集成化。

## 如何：学习如何在Arduino中读取命令行参数

在Arduino中，我们可以使用 `Serial` 库来读取命令行参数。首先，我们需要确保Arduino板与电脑连接，然后打开串口监视器。接下来，我们可以使用 `Serial.available()` 函数来检测是否有数据可读取，然后使用 `Serial.read()` 函数来读取每一个字符。最后，我们可以将读取到的命令行参数存储到变量中并在代码中使用它们。

```Arduino
int num1, num2, result;
Serial.begin(9600);

if (Serial.available()) {
  num1 = Serial.read() - '0'; // 将字符转换为数字
  num2 = Serial.read() - '0';
}

result = num1 + num2;
Serial.print("结果为：");
Serial.println(result);
```

运行上述代码后，在串口监视器中输入两个数字并发送，即可得到它们的和作为结果。

## 深入了解：关于读取命令行参数的更多信息

除了上述简单的读取命令行参数的方法外，我们还可以使用 `Serial.parseInt()` 函数来直接将输入的参数转换为整数。此外，我们还可以通过输入分隔符来读取多个参数，例如使用空格 ` ` 或逗号 `,` 分隔。更多关于如何读取和处理命令行参数的具体方法可以通过以下链接学习。

## 参考链接

- [Arduino官方文档 - Serial](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [Arduino官方教程 - 简易命令行参数接口](https://www.arduino.cc/en/Tutorial/CommandLineArguments)
- [Arduino命令行参数解析工具库 - CommandParser](https://github.com/davetcc/CommandParser)