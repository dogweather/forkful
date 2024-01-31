---
title:                "写入标准错误"
date:                  2024-01-19
html_title:           "Arduino: 写入标准错误"
simple_title:         "写入标准错误"

category:             "Arduino"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
在Arduino中编写到标准错误意味着将错误信息或调试信息发送到串行监视器，而非常规输出。程序员这样做是为了调试程序和分离正常数据与错误信息，方便问题追踪。

## 如何：
```Arduino
void setup() {
  Serial.begin(9600); // 初始化串行通信
  while (!Serial) {
    ; // 等待串行端口连接
  }
}

void loop() {
  Serial.println("普通信息。"); // 标准输出
  Serial.println("错误信息！", STDERR); // 标准错误输出，在Arduino里没有定义STDERR，仅为展示
}
```
样本输出：
```
普通信息。
错误信息！
```
Arduino IDE没有直接支持标准错误流，上面"STDERR"的用法仅为示例，并非真实代码。

## 深入探究
Arduino没有内置处理标准错误流的方式，因为它主要用于嵌入式系统，这些系统通常不需要复杂的错误处理。过去，程序员只能利用LED状态或简单的串行输出来调试。现在，最常见的替代方法是使用串行输出分辨错误。实际上，我们通过特定标记（如在消息前添加"ERROR: "）来区分错误信息。

## 参考
- Arduino官方文档: https://www.arduino.cc/reference/en/
- 教程：“如何使用Arduino进行调试”: https://www.arduino.cc/en/Tutorial/BuiltInExamples
- 论坛: https://forum.arduino.cc/
