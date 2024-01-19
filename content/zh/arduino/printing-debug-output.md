---
title:                "打印调试输出"
html_title:           "Clojure: 打印调试输出"
simple_title:         "打印调试输出"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## 什么和为什么？
在 Arduino 编程中，打印调试输出是程序员运行代码时，将指定的信息或变量值输出到串行监视器的一种方法。我们这么做，是为了查错和验证代码的正确性。

## 如何做到：
```Arduino
void setup() {
  Serial.begin(9600); // 初始化串行通信
}

void loop() {
  Serial.println("这是一条调试信息"); // 打印调试信息
  delay(1000); // 延迟1秒
}
```
在Arduino串行监视器中，你将看到每隔一秒就输出“这是一条调试信息”。

## 深入探索
调试技术已经存在很多年了，可以追溯到计算机编程的早期阶段，如今被广泛应用于各类编程语言中。在 Arduino 中，除了`Serial.println()`之外，还有一些其他方法可以产生调试输出。例如，`Serial.print()`, `Serial.write()`，和`Serial.printf()`等。这些函数的主要区别在于它们的输出格式和处理数据的方式。

你可能会疑惑为什么我们要使用9600这个数字来初始化Serial对象。这是因为9600是串行通信中较为常见的波特率（baud rate），代表每秒传送的比特数。

## 延伸阅读
1. [Arduino官方参考文档 - Serial.begin()](https://www.arduino.cc/reference/en/language/functions/communication/serial/begin/)
2. [Arduino官方参考文档 - Serial.println()](https://www.arduino.cc/reference/en/language/functions/communication/serial/print/)
3. [Arduino官方参考文档 - Serial.print()](https://www.arduino.cc/reference/en/language/functions/communication/serial/println/)