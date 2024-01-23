---
title:                "处理 CSV 文件"
html_title:           "Bash: 处理 CSV 文件"
simple_title:         "处理 CSV 文件"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
什么是CSV？为什么处理CSV？ CSV（逗号分隔值）文件是存储表数据的简单格式。程序员经常与CSV打交道来读写数据，因其格式简单，易于理解和编辑。

## How to:
在Arduino中读取CSV，通常用于解析来自存储设备的数据。如下例子展示如何逐行读取并解析CSV文件数据。

```Arduino
#include <SPI.h>
#include <SD.h>

File myFile;

void setup() {
  Serial.begin(9600);
  if (!SD.begin(10)) {
    Serial.println("SD card initialization failed!");
    return;
  }
  
  myFile = SD.open("data.csv");
  if (myFile) {
    while (myFile.available()) {
      String dataLine = myFile.readStringUntil('\n');
      Serial.println(dataLine);
    }
    myFile.close();
  } else {
    Serial.println("Error opening data.csv");
  }
}

void loop() {
  // 这里不需要代码，因只在setup中读取文件
}
```

输出样例：
```
sensor1, value1
sensor2, value2
sensor3, value3
```

## Deep Dive
历史背景：CSV格式自1970年代被广泛使用，因其兼容性和简易性，它成为交换表格数据的常见方式。替代格式包括XML和JSON，这些格式能提供更复杂的数据结构。在Arduino中处理CSV通常涉及字符串分割和类型转换，但也受限于设备的存储和处理能力。

## See Also
- Arduino官网文档: [SD库](https://www.arduino.cc/en/Reference/SD)
- 进一步了解CSV格式: [RFC 4180](https://tools.ietf.org/html/rfc4180)
