---
title:                "创建临时文件"
html_title:           "Kotlin: 创建临时文件"
simple_title:         "创建临时文件"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 是什么以及为什么？

创建临时文件是编程中的一个常用操作，用以储存短期内需要但之后可以丢弃的数据。程序员创建临时文件的主要原因是减少内存消耗，特别是对于大量短期数据。

## 如何实现：

Arduino不直接提供创建临时文件的功能，但是我们可以在SD卡上通过新建文件的方式模拟创建临时文件。以下是代码示例：

```Arduino 
#include <SD.h>

File tmpFile;

void setup() {
  Serial.begin(9600);
  
  if (!SD.begin(4)) {
    Serial.println("Initialization Failed!");
    return;
  }
  tmpFile = SD.open("tmpfile.txt", FILE_WRITE);
  if (tmpFile) {
    tmpFile.println("This is a temp file!");
    tmpFile.close();
    Serial.println("Temp file created.");
  } else {
    Serial.println("Error opening tmpfile.txt");
  }
}

void loop() {
  
}
```
在上述程序中，你会创建一个名为 "tmpfile.txt" 的临时文件。数据写入后，你可以随时删除这个文件来释放SD卡空间。

## 深入解读

从历史角度讲，临时文件的概念始于早期的机械计算机时代。当时的计算机内存极其有限，因此临时文件成为一种有效存储解决方案。

在Arduino环境中，我们并没有更多的选项来实现临时文件的作用。但是，你可以选择将数据存储在类似SRAM或EEPROM等内部存储器中。值得注意的是，存储空间也有限且经常使用可能会引发磨损问题。

关于实现细节，Arduino SD库为我们提供了一套非常实用的文件操作API，我们可以像操作计算机的文件系统那样操作SD卡中的文件。

## 参考资源

了解更多Arduino的SD卡操作，参考这里：[Arduino SD library](https://www.arduino.cc/en/Reference/SD)

想要知道更多关于临时文件的信息？看这里：[Temporary files](https://en.wikipedia.org/wiki/Temporary_file)

对SRAM和EEPROM有求知欲？参考这里：[EEPROM vs SRAM](https://www.geekhideout.com/arduino-eeprom-vs-sram.shtml)