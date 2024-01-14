---
title:    "Arduino: 检查目录是否存在"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# 为什么会检查目录是否存在

在编程中，有时候我们需要检查某个目录是否存在。这可能是因为我们希望在某个目录下存储文件，或者需要读取已经存在的文件。通过检查目录是否存在，我们可以避免出现错误或者程序崩溃的情况。

# 如何检查目录是否存在

在Arduino编程中，我们可以使用File类的exists()函数来检查目录是否存在。下面是一个简单的代码示例：

```Arduino

#include <SPI.h>
#include <SD.h>

File myDir;

void setup() {
  Serial.begin(9600);
  
  // 初始化SD卡
  if (!SD.begin(4)) {
    Serial.println("SD卡初始化失败！");
    return;
  }

  // 在SD卡根目录下创建一个名为"data"的新目录
  myDir = SD.mkdir("data");
}

void loop() {
  // 检查目录"data"是否存在
  if (myDir.exists()) {
    Serial.println("目录data存在！");
  }

  // 等待1秒
  delay(1000);
}

```

代码执行后，如果目录"data"存在，简单的串口输出会显示"目录data存在！"。如果目录不存在，则什么都不会输出。

# 深入了解检查目录是否存在

在Arduino中，我们通过File类的exists()函数来检查目录是否存在。这个函数会根据目录的路径返回一个布尔值，若目录存在则返回true，否则返回false。值得注意的是，这个函数只能检查目录是否存在，无法判断目录是否可读取或者可写入。

# 参考资料

- Arduino文档：https://www.arduino.cc/reference/en/libraries/sd/
- SD库参考手册：https://www.arduino.cc/en/Reference/SD
- File类参考手册：https://www.arduino.cc/en/Reference/File