---
title:                "阅读文本文件"
html_title:           "Arduino: 阅读文本文件"
simple_title:         "阅读文本文件"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 什么是文本文件读取? 
文本文件读取是指从计算机中读取文本文件的过程。这项技术通常由程序员使用，允许他们从外部来源获取信息，如Web或存储设备。

## 如何做到: 
```Arduino
// 读取文本文件示例
void setup() {
  // 打开文本文件
  File textFile = SD.open("example.txt");
  // 如果文件存在并成功打开
  if (textFile) {
    // 逐行读取文件内容
    while (textFile.available()) {
      // 输出每行内容到串口监视器
      Serial.println(textFile.readStringUntil('\n'));
    }
    // 关闭文件
    textFile.close();
  }
}
```

```
示例输出:
This is line 1 of the text file.
This is line 2 of the text file.
This is line 3 of the text file.
```

## 深入了解:
文本文件读取是一种比较常用的技术，最初用来读取硬盘驱动器中保存的文本文档。而如今，随着相应技术的发展，我们可以通过各种外部设备如SD卡、USB设备或网络来读取文本文件。若要读取非文本文件，比如图片或视频，我们可以通过使用相应的库来实现。

## 参考文献:
- Arduino官方文档：https://www.arduino.cc/reference/en/libraries/sd/
- 文本文件读取示例代码：https://www.arduino.cc/en/Tutorial/ReadASCIIString
- 更多关于文本文件读取的资料：https://www.i-programmer.info/programming/arduino/6668-c-arduino-and-text-files.html