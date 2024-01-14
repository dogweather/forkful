---
title:                "Arduino: 打印调试输出"
simple_title:         "打印调试输出"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

为什么：打印调试输出信息对于Arduino编程者来说是非常重要的。通过输出调试信息，我们可以更有效地检查代码中的错误，提高程序的稳定性和可靠性。

如何操作：在Arduino编程中，我们可以使用Serial库来进行调试输出。首先，我们需要在代码开头部分使用```Serial.begin()```来初始化串口通信。然后，我们可以在代码中用```Serial.println()```来输出需要调试的信息。例如：

```Arduino
int a = 5;
int b = 10;
int c = a + b;
Serial.begin(9600);
Serial.println("The value of a is: ");
Serial.println(a);
Serial.println("The value of b is: ");
Serial.println(b);
Serial.println("The value of c is: ");
Serial.println(c);
```

这样，我们就可以通过串口监视器来查看输出的调试信息，从而检查代码中是否有错误。

深入了解：除了使用```Serial.println()```来输出文本信息外，我们还可以使用```Serial.print()```来输出数字、字符和其他数据类型。此外，我们还可以使用```Serial.write()```来直接输出字节数据。

此外，我们还可以使用自定义的调试宏来简化调试输出的代码。例如，可以定义一个名为```DEBUG```的宏来控制调试输出的开关。在代码中，我们可以使用```#ifdef```和```#endif```来控制当DEBUG宏被定义时才执行调试输出的代码。这样，在发布正式版本时，只需要将DEBUG宏置为未定义状态，就可以自动屏蔽调试输出，从而提高代码的运行效率。

另外，我们还可以在调试输出中加入时间戳和调试级别等信息，以便更好地定位和分析代码中的错误。

此外，对于一些特殊的调试需求，我们还可以使用专门的调试工具来辅助调试输出，如使用Oscilloscope来查看模拟信号、使用Logic Analyzer来查看数字信号等。

总之，打印调试输出信息可以帮助我们更有效地进行代码调试，提高程序的可靠性和稳定性。

另请参阅：
- [使用Serial库进行调试输出](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [使用自定义调试宏简化调试输出](https://www.arduino.cc/en/Tutorial/Debugging)
- [使用专用调试工具辅助调试](https://www.electronicwings.com/arduino/debugging-techniques-in-arduino)

参阅：https://markdown-it.github.io/