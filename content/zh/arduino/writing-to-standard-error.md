---
title:    "Arduino: 写入标准错误"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# 为什么会想要写错误标准输出？

当我们在进行Arduino编程时，我们可能会遇到一些错误。这些错误可能是因为语法错误、逻辑错误或其他错误。如果我们没有正确处理这些错误，我们的程序可能会崩溃或出现不可预知的结果。在这种情况下，写入标准错误输出可以帮助我们定位并解决这些错误，从而提高我们的程序的稳定性和可靠性。

## 如何实现

在Arduino中，我们可以使用Serial.print()函数来向串行监视器输出信息。但是，有时我们可能需要将信息输出到标准错误输出。为此，我们可以使用Serial.println(F("error message")); 语句来输出错误消息。F()函数可以将字符串存储到程序存储器中，而不是RAM中，从而节省内存。我们还可以使用Serial.write()函数来向标准错误输出写入单独的字符。

下面是一个简单的示例代码，展示了如何使用标准错误输出来处理错误：

```
#include <Arduino.h>

void setup() {
  Serial.begin(9600); // 初始化串口通信
}

void loop() {
  int sensorValue = analogRead(A0); // 读取来自传感器的数据
  if (sensorValue < 50) { // 如果传感器值小于50
    Serial.println(F("ERROR: Sensor value too low")); // 输出错误消息
  }
  delay(1000); // 延迟1秒钟
}
```

在这个例子中，如果传感器的值低于50，程序会输出“ERROR: Sensor value too low”消息到标准错误输出。

## 深入了解

写入标准错误输出还有其他一些用处。例如，在调试过程中，我们可以在关键的代码部分添加一些输出语句，从而能够查看程序在运行时的状态。这有助于我们更快地定位并解决错误。

此外，我们还可以通过添加条件来控制标准错误输出。例如，我们可以使用if语句来检查程序的状态并根据需要输出相应的错误消息。

# 参考链接

- [Arduino官方文档：Serial.print()函数](https://www.arduino.cc/reference/en/language/functions/communication/serial/print/)
- [Arduino官方文档：Serial.println()函数](https://www.arduino.cc/reference/en/language/functions/communication/serial/println/)
- [Arduino官方文档：Serial.write()函数](https://www.arduino.cc/reference/en/language/functions/communication/serial/write/)

# 参见

- [Markdown语法指南](https://www.markdownguide.org/)
- [Arduino示例代码](https://www.arduino.cc/en/Tutorial/HomePage)