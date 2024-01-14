---
title:    "Arduino: 打印调试输出"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

# 为什么要打印调试输出

在Arduino编程中，打印调试输出是一种用于调试程序的常用方法。通过在代码中插入打印语句，程序员可以在运行时观察变量的值和程序的执行流程，从而更容易发现问题所在。

# 如何打印调试输出

要打印调试输出，首先需要使用Serial库。在代码开头，插入以下代码：

```Arduino
#include <Serial.h> 
```

然后，在setup()函数中，初始化串口： 

```Arduino 
void setup() { 
    Serial.begin(9600); 
} 
```

最后，在需要打印的地方，使用Serial.print()或Serial.println()函数来输出变量的值或文本信息。例如，如果需要打印一个变量的值，可以使用以下代码：

```Arduino
int num = 10; 
Serial.print("The value of num is: "); 
Serial.println(num);
```

输出结果将会是：The value of num is: 10 

# 深入了解打印调试输出

除了Serial.print()和Serial.println()函数外，还有一些其他的打印调试输出函数可以使用。比如，Serial.write()函数可以打印一个单独的字符，Serial.print()函数还可以指定输出的进制方式，例如：

```Arduino
Serial.print(100, HEX); // 输出100的十六进制表示：64
```

此外，还可以通过设置串口的波特率来调整打印输出的速度。通常，默认的波特率为9600，但是也可以根据需要调整为更高或更低的值。

# 参考资料

- [Arduino官方文档-Serial](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [How to Use Serial Print in Arduino](https://www.arduino.cc/en/Tutorial/SerialPrint)
- [Debugging in Arduino](https://www.electronicshub.org/debugging-in-arduino/)

# 另请参阅

- [Arduino编程基础教程](https://www.arduino.cc/en/Tutorial/BuiltInExamples)
- [Arduino中文社区](https://www.arduino.cn/)