---
title:                "使用正则表达式"
html_title:           "Arduino: 使用正则表达式"
simple_title:         "使用正则表达式"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？

正则表达式是查找和替换文本的强大工具。程序员使用此工具处理复杂的搜索条件和模式替换。

## 如何做：

以下是在Arduino中使用正则表达式的示例：

```Arduino
#include <Regexp.h>

char msg[64] = "temp=24.2_hum=46.6";
MatchState ms;

void setup() {
  Serial.begin(9600);
  ms.Target(msg);

  char buf[10];

  ms.Match("temp=([-0-9.]+)_hum=([-0-9.]+)");
  ms.GetCapture(buf, 0);
  float temp = atof(buf);
  
  ms.GetCapture(buf, 1);
  float hum = atof(buf);

  Serial.println(temp);
  Serial.println(hum);
}

void loop() {

}
```

上述代码中的"temp=([-0-9.]+)_hum=([-0-9.]+)"是一个正则表达式。它可以解析字符串"temp=24.2_hum=46.6"从而得到温度和湿度的数值。

## 深度挖掘：

正则表达式起源于20世纪50年代，作为一种操纵复杂字符模式的算法。用于Arduino的Regexp库，是这个历史概念在嵌入式系统环境中的较新实现。

选用正则表达式的原因是它可以处理非常复杂的搜索和替换情况。然而，对于简单的文本搜索和替换，"String.replace"函数就足够了，它在处理简单情况时更为高效。

在Arduino中使用正则表达式需要`Regexp`库，否则，Arduino不支持正则表达式。该库是为Arduino编写的轻量级正则表达式引擎，可用于解析接收到的数据。

## 参考资料：
* Arduino官方文档（https://www.arduino.cc/reference/en/）
* Regexp库的源代码（https://github.com/nickgammon/Regexp）
* 在线正则表达式测试（https://regex101.com/）