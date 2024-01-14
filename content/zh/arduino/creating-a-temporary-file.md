---
title:    "Arduino: 创建临时文件"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 为什么

在Arduino编程中，创建临时文件可以帮助我们暂时储存数据或者执行某些特定的任务。这通常可以提高我们程序的效率和可读性。

## 如何做

首先，我们需要在程序的开头引入SD卡库，并初始化一个文件对象。例如：

```Arduino
#include <SD.h>

File temporaryFile;
```

接着，在 ```setup()```函数里，我们可以使用 ```SD.begin()```来启动SD卡并检测是否成功。然后，通过 ```SD.open()```函数，我们可以创建一个临时文件并定义文件名和操作模式。例如，创建一个名为“tempFile.txt”的文件，以读写模式打开：

```Arduino
if (SD.begin(10)) {
  temporaryFile = SD.open("tempFile.txt", FILE_WRITE);
}
```

此外，我们也可以在创建临时文件的同时写入一些数据。例如：

```Arduino
if (SD.begin(10)) {
  temporaryFile = SD.open("tempFile.txt", FILE_WRITE);
  temporaryFile.println("This is a temporary file.");
}
```

最后，在我们程序执行完毕后，我们需要关闭文件以释放内存。这也可以通过调用 ```temporaryFile.close();```来实现。完整的代码示例如下：

```Arduino
#include <SD.h>

File temporaryFile;

void setup() {
  if (SD.begin(10)) {
    temporaryFile = SD.open("tempFile.txt", FILE_WRITE);
    if (temporaryFile) {
      temporaryFile.println("This is a temporary file.");
      temporaryFile.close();
    }
  }
}

void loop() {

}
```

## 深入了解

创建临时文件不仅可以用来储存数据，它还可以用来存储一些特定的额外信息。例如，在某些情况下，我们可能会创建一个文件来记录系统的运行状态或者错误信息。这样，在调试和故障排查时，我们可以通过读取这个文件来找出问题所在。

此外，我们还可以通过使用 ```SD.exists()```函数来检查文件是否存在，以及 ```SD.remove()```函数来删除文件。

## 参考链接

- [https://www.arduino.cc/en/Reference/SD](https://www.arduino.cc/en/Reference/SD)
- [http://www.circuitstoday.com/interfacing-microcontrollers-with-sd-card](http://www.circuitstoday.com/interfacing-microcontrollers-with-sd-card)
- [https://learn.adafruit.com/adafruit-micro-sd-breakout-board-card-tutorial/arduino-library-use](https://learn.adafruit.com/adafruit-micro-sd-breakout-board-card-tutorial/arduino-library-use)

## 参见