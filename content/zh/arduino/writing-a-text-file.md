---
title:                "Arduino: 编写文本文件"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 为什么

编写文本文件在Arduino编程中是一个常见的任务。通过将数据保存到文本文件，我们可以轻松地在内存和外部存储器之间传输数据，并在需要时将其读取回来。此外，文本文件也可以用来保存我们的程序的设置和状态，使得程序更加灵活和可靠。

## 如何

编写一个文本文件需要使用Arduino的文件系统命令。首先，我们需要在```setup()```函数中使用```SD.begin()```命令来初始化SD卡或外部存储设备。然后，我们可以使用```SD.open()```命令来打开一个具有指定名称和文件模式（如读、写或追加）的文本文件。接下来，我们可以使用```<<```操作符来将数据写入文件，最后使用```close()```命令来关闭文件。

下面是一个例子，它将一个字符串写入名为"data.txt"的文本文件，然后重新打开该文件并将其内容读取出来：

```Arduino
#include <SPI.h>
#include <SD.h>

const int chipSelect = 4;

void setup() {
  Serial.begin(9600);
  while (!Serial) {
    ; // 等待串口连接
  }

  if (!SD.begin(chipSelect)) {
    Serial.println("SD 卡初始化失败");
    return;
  }

  File dataFile = SD.open("data.txt", FILE_WRITE);

  if (dataFile) {
    dataFile << "Hello World!" << endl;
    dataFile.close();
    Serial.println("文件已写入");
  } else {
    Serial.println("无法打开文件");
  }

  dataFile = SD.open("data.txt");

  if (dataFile) {
    while (dataFile.available()) {
      Serial.write(dataFile.read());
    }
    dataFile.close();
    Serial.println("文件已读取");
  } else {
    Serial.println("无法打开文件");
  }
}

void loop() {
  // 空函数，仅初始化一次
}
```

运行上面的代码后，串口监视器将会输出：

```
文件已写入
文件已读取
Hello World!
```

## 深入了解

在编写文本文件时，需要注意一些问题。首先，我们需要选择适当的文件模式来打开文件，以确保数据被正确写入或读取。此外，如果文件已经存在，我们可以选择追加模式来将数据附加到文件的末尾，而不是覆盖文件中的数据。其次，由于文件系统有限的写入次数，我们需要小心谨慎地使用文件写入命令，避免过多的写入操作导致SD卡损坏。

## 参考资料

- [Arduino SD库参考手册](https://www.arduino.cc/reference/en/libraries/sd/)
- [使用Arduino写入文本文件](https://create.arduino.cc/projecthub/rafael-aurelio/create-a-text-file-using-arduino-4c855f)

## 参见

- [Arduino编程入门教程](https://www.arduino.cc/en/Tutorial/HomePage)
- [外部存储器和Arduino数据传输指南](https://www.arduino.cc/en/Guide/ArduinoSD)