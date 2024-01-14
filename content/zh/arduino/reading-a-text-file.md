---
title:    "Arduino: 阅读文本文件"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

为什么：为什么要阅读文本文件？阅读文本文件可以帮助开发人员更有效地存储和处理数据。

如何：在Arduino编程中，使用`SD`库可以轻松地读取文本文件。首先，我们需要将SD卡插入到Arduino板的SD卡槽中。接下来，在我们的程序中，我们需要包含`SD`库并设置SD卡的引脚。然后，我们可以使用`SD.open()`函数来打开文本文件，使用`SD.read()`函数逐行读取文件中的数据，并使用`SD.close()`函数来关闭文件。最后，我们可以使用Serial监视器来查看读取到的数据。以下是一个简单的例子：

```Arduino
#include <SD.h>

const int chipSelect = 10; //设置SD卡的引脚
File myFile;

void setup() {
  Serial.begin(9600); //初始化串口
  while (!Serial) {
    ; //等待串口连接完成
  }

  Serial.print("查找SD卡...");
  if (!SD.begin(chipSelect)) { //初始化SD卡
    Serial.println("无法找到SD卡");
    return;
  }
  Serial.println("成功");
}

void loop() {
  myFile = SD.open("data.txt"); //打开文本文件
  if (myFile) {
    while (myFile.available()) { //当文件可用时
      Serial.write(myFile.read()); //读取文件中的数据并输出到串口
    }
    myFile.close(); //关闭文件
  } else {
    Serial.println("无法打开文件");
  }
}
```

输出：
```
1
2
3
4
5
6
```

深入了解：除了使用`SD`库外，我们还可以使用`SPI`库来读取文本文件。`SPI`库允许我们通过SPI总线来访问SD卡，能够提高读取速度。我们可以使用`SPI.transfer()`函数来逐字节传输数据，并使用`digitalWrite()`函数来控制CS引脚。以下是一个示例：

```Arduino
#include <SPI.h>

File myFile;
uint8_t b1;

void setup() {
  Serial.begin(9600); //初始化串口
  while (!Serial) {
    ; //等待串口连接完成
  }

  digitalWrite(SS, HIGH); //设置CS引脚为高电平
  SPI.begin(); //初始化SPI总线
  myFile = SD.open("data.txt"); //打开文本文件
}

void loop() {
  while (myFile.available()) { //当文件可用时
    b1 = myFile.read(); //读取一个字节的数据
    Serial.write(b1); //输出到串口
  }
  myFile.close(); //关闭文件
}
```

输出：
```
1
2
3
4
5
6
```

 请注意，在使用`SPI`库时，我们需要手动设置CS引脚和初始化SPI总线。此外，使用`SPI`库也可以对SD卡进行更多的控制，如读取SD卡的状态和写入数据。更多关于SPI库的介绍，请参考[官方文档](https://www.arduino.cc/en/Reference/SPI)。

## 参考文献：
[Arduino - SD](https://www.arduino.cc/en/Reference/SD)\
[Arduino - SPI](https://www.arduino.cc/en/Reference/SPI)

## 参考链接：
[官方文档 - SD](https://www.arduino.cc/en/Reference/SD)\
[官方文档 - SPI](https://www.arduino.cc/en/Reference/SPI)