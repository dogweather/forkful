---
title:                "开始新项目"
html_title:           "Lua: 开始新项目"
simple_title:         "开始新项目"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?

开始一个新项目是选择一种特定的编程任务并构建你的代码框架的过程，程序员这样做是因为他们需要一个稳定、有组织的方式来管理他们的代码。

## 如何: 

下面的代码块将展示如何在Arduino上开启一个新项目并进行一些基本设置。 

```Arduino
// Arduino例程 - 闪烁LED

int ledPin = 13;                 // LED attached to pin 13

void setup() {                   

  pinMode(ledPin, OUTPUT);       // declare pin 13 as output
  
}

void loop() {                     
  digitalWrite(ledPin, HIGH);    // turn the LED on
  delay(1000);                   // wait for a second
  digitalWrite(ledPin, LOW);     // turn the LED off
  delay(1000);                   // wait for a second
}
```

在上述代码的执行过程中，一个连接在13号引脚处的LED会被点亮，等待一秒钟，然后被关闭，再等待一秒钟。 

## 深入探究 

在处理Arduino项目时，理解其历史背景、考虑可能的替代方案以及实施细节是至关重要的，以便你可以更深入地理解何时以及如何使用它。

历史背景：Arduino 于2005年在意大利伊维雷亚出生，是为艺术家、设计师、爱好者和任何对创建交互式对象或环境感兴趣的人提供一个便宜、简便的工具。

替代方案：虽然Arduino是一个很好的开发环境，但根据你的项目和需求，也许还有其它的解决方案可供选择，如Raspberry Pi、BeagleBone Black以及Teensy等。

实施细节：你的代码在每次上传到板子时都会编译，它转换成了Arduino理解的微型指令，然后通过USB或其他接口进行通信。

## 参考资料 

1. Arduino官方文档 - https://www.arduino.cc/
2. Arduino项目库 - https://create.arduino.cc/projecthub
3. Arduino教程 - https://www.arduino.cc/en/Tutorial/HomePage

这些链接都包含了有关使用Arduino进行项目编程的更多信息。