---
title:                "检查目录是否存在"
html_title:           "Arduino: 检查目录是否存在"
simple_title:         "检查目录是否存在"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 什么以及为什么？
检查目录是否存在是在使用文件系统时，判断特定的文件目录是否已经存在的过程。这对于避免在不存在的目录中创建文件或存储数据至关重要。

## 怎么做：
```Arduino
#include <SPI.h>
#include <SD.h>

void setup() {
  Serial.begin(9600);

  if (!SD.begin(4)) {
    Serial.println("Initialization failed!");
    return;
  }

  if (SD.exists("/test")) {
    Serial.println("Directory exists.");
  } else {
    Serial.println("Directory does not exist.");
  }
}

void loop() {
  // nothing here
}
```
串行监视器的输出结果：
```Arduino
Directory exists.
```
如果你的 '/test' 目录存在。如果目录不存在，你将会看到：
```Arduino
Directory does not exist.
```

## 深入了解
在Arduino的早期版本里，进行目录检查的功能并没有被包含进SD库。随着开发者对丰富文件处理功能的需求增加，这个功能被后续添加进去。 你也可以换种方式实现，例如尝试在目录里创建文件，如果失败则表明目录不存在。尽管有效，但这种方式并不被推荐，因为可能会导致不必要的文件创建。 

## 另请参见
- Arduino SD 库官方说明：https://www.arduino.cc/en/Reference/SD
- Arduino 文件系统指南：https://www.arduino.cc/en/Guide/Examples#fullFileIO