---
title:                "检查目录是否存在"
aliases:
- zh/arduino/checking-if-a-directory-exists.md
date:                  2024-02-03T19:06:39.583938-07:00
model:                 gpt-4-0125-preview
simple_title:         "检查目录是否存在"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么和为什么？
在Arduino编程的背景下，检查SD卡或类似存储模块上是否存在目录，允许您在不出错的情况下读取或写入文件。这个操作对于数据记录、配置管理或任何需要结构化文件存储的任务至关重要，确保了应用程序中的可靠性和流畅性能。

## 如何操作：
Arduino默认不支持复杂的文件系统操作。但是，通过使用SD库，它是标准Arduino IDE的一部分，您可以轻松地处理文件和目录。要检查目录是否存在，首先需要初始化SD卡，然后使用SD库中的`exists()`方法。

首先，包含SD库并声明芯片选择引脚：

```cpp
#include <SPI.h>
#include <SD.h>

const int chipSelect = 4; // SD卡模块的芯片选择引脚
```

在`setup()`函数中，初始化SD卡并检查目录是否存在：

```cpp
void setup() {
  Serial.begin(9600);
  
  if (!SD.begin(chipSelect)) {
    Serial.println("初始化失败！");
    return;
  }

  // 检查目录是否存在
  if (SD.exists("/myDir")) {
    Serial.println("目录存在。");
  } else {
    Serial.println("目录不存在。");
  }
}
```
在`loop()`函数中，您可以根据需要保持它为空或添加其他操作代码：

```cpp
void loop() {
  // 操作代码或保持空
}
```

运行代码时的示例输出将是：

```
目录存在。
```
或

```
目录不存在。
```

确保SD卡被正确格式化以及`/myDir`目录路径符合您的特定需求非常重要。这个基本检查是在SD卡上使用Arduino执行更复杂的文件和目录操作的基石。
