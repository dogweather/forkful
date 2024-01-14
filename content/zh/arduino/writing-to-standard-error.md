---
title:                "Arduino: 写入标准错误"
simple_title:         "写入标准错误"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# 为什么要写入标准错误

在Arduino编程中，我们经常需要进行调试和错误排查。而将信息写入标准错误输出可以帮助我们更容易地发现程序中的问题，从而提高我们的代码质量。

## 如何写入标准错误

要在Arduino中写入标准错误，您需要使用`Serial.print()`函数，并将所需信息作为参数传递给它。例如，`Serial.print("Error occurred");`将会输出"Error occurred"到标准错误输出。

```Arduino
void setup(){
  Serial.begin(9600); // 初始化序列通信
}

void loop(){
  // 代码的其他部分
  if (error occurred){
    Serial.print("Error occurred"); // 写入标准错误输出
  }
}
```

在Arduino中，标准错误输出默认被重定向到串行监视器，因此您可以通过打开串行监视器来查看输出结果。

## 深入了解写入标准错误

通过使用`Serial.print()`函数写入标准错误，我们可以将各种信息输出到调试窗口。除了文本信息外，我们还可以输出变量值、数组元素等。此外，Arduino还提供了`Serial.println()`函数，它会在每次输出文本后自动换行，更方便我们阅读输出结果。

```Arduino
int sensorValue = 100; // 传感器值
Serial.print("Sensor value:"); // 输出文本
Serial.println(sensorValue); // 输出变量值并换行
```

请注意，当您想要在程序中保留标准输出时，应避免使用`Serial.print()`函数。相反，您可以使用`Serial.write()`函数来将信息写入标准输出。

## 参考链接

- [Arduino官方文档：使用Serial对象](https://www.arduino.cc/reference/zh/serial/)
- [收集Arduino错误的技巧](https://www.arduino.cc/en/Main/CollectErrorLogs/)
- [在Arduino代码中使用标准错误](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [使用Arduino进行调试](https://www.arduino.cc/en/Tutorial/Debugger/)

# 查看更多

- [如何使用Serial Monitor进行调试](https://www.arduino.cc/en/Tutorial/BuiltInExamples/SerialCallResponse/)
- [Arduino调试技巧：使用断言来定位错误](https://www.arduino.cc/en/Reference/Assertion/)
- [调试您的Arduino代码：通过重新定义new和delete函数](https://www.arduino.cc/en/Tutorial/Assert/)