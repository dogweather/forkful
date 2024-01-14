---
title:    "Arduino: 檢查目錄是否存在"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

#为什么
您是否曾经想过如何在Arduino编程中检测目录是否存在？这篇博客将向您解释为什么需要这样做以及如何实现它。继续阅读以了解更多信息。

#如何
在Arduino中，您可以使用exists（）函数来检查目录是否存在。让我们来看一个示例代码：

```Arduino
#include <SD.h>

void setup() {
  Serial.begin(9600);

  if (SD.exists("/test")) {
    Serial.println("The directory test exists.");
  } else {
    Serial.println("The directory test does not exist.");
  }
}

void loop() {

}
```
该代码将打开一个串行监视器，并检查是否存在名为“test”的目录。如果存在，则会打印出相应的消息，如果不存在，则会打印不同的消息。

##深入了解
通过使用exists（）函数，您可以在Arduino编程中轻松检查目录是否存在。此函数需要一个字符串作为参数，该字符串是要检查的目录名称。它将返回一个布尔值，如果目录存在，则返回true，如果目录不存在，则返回false。

您也可以将exists（）函数与其他函数结合使用，例如mkdir（）函数来创建一个新的目录。如果您想要在创建之前检查目录是否已经存在，那么这将非常有用。

#参见
- [Arduino文档：SD.exists（）函数](https://www.arduino.cc/en/Reference/SDexists)
- [How To Check If a File or Directory Exists in C++](https://www.digitalocean.com/community/tutorials/how-to-check-if-a-file-or-directory-exists-in-c-or-cpp)
- [Arduino编程基础知识](https://www.arduino.cc/en/Tutorial/Foundations)