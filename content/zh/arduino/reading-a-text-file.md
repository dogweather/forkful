---
title:                "Arduino: 读取文本文件"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

# 为什么要读取文本文件

阅读文本文件是用来从外部资源获取数据的重要方法。它可以让您的Arduino项目与其他设备或网络通信，并从中获取数据。这是一个非常有用的功能，可以让您的项目变得更加智能和灵活。

## 如何读取文本文件

读取文本文件的方法与读取其他类型的文件类似，可以通过Arduino的SD库来实现。首先，您需要将文本文件保存在SD卡中，然后使用```SD.open()```函数来打开文件。接下来，可以使用```readLine()```函数来逐行读取文件中的内容，并将其存储在变量中。最后，使用```close()```函数来关闭文件。

下面是一个简单的示例代码，演示了如何读取文本文件并将其打印到串行监视器中。

```
#include <SD.h>

File myFile;

void setup() {
  // 初始化串行监视器
  Serial.begin(9600);

  // 初始化SD卡
  SD.begin(10);

  // 打开文本文件
  myFile = SD.open("data.txt");

  // 读取文件内容并打印到串行监视器
  while (myFile.available()) {
    Serial.println(myFile.readStringUntil('\n'));
  }

  // 关闭文件
  myFile.close();
}

void loop() {
  // 空
}
```

运行上面的代码后，您将在串行监视器中看到文本文件中的内容。

## 深入了解文本文件的读取

虽然读取文本文件的方法比较简单，但是深入了解其原理可以让您更好的掌握文件操作。首先，需要知道的是文本文件是由一系列字符组成的，每行以换行符或回车符结尾。因此，使用```readLine()```函数可以逐行读取文件中的内容。另外，您还可以使用```parseInt()```和```parseFloat()```函数来将读取的字符转换为整数或浮点数。

此外，您还可以使用逗号分隔符来将每一行中的不同数据分开读取，并存储在数组中。

# 参考链接

- [SD卡读写教程](https://www.arduino.cc/en/Reference/SD)
- [文本文件读取示例](https://www.arduino.cc/en/Tutorial/ReadASCIIString)
- [SD库参考手册](https://www.arduino.cc/en/Reference/SD)