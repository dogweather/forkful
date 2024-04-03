---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:39.583938-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Arduino\u9ED8\u8BA4\u4E0D\u652F\u6301\
  \u590D\u6742\u7684\u6587\u4EF6\u7CFB\u7EDF\u64CD\u4F5C\u3002\u4F46\u662F\uFF0C\u901A\
  \u8FC7\u4F7F\u7528SD\u5E93\uFF0C\u5B83\u662F\u6807\u51C6Arduino IDE\u7684\u4E00\u90E8\
  \u5206\uFF0C\u60A8\u53EF\u4EE5\u8F7B\u677E\u5730\u5904\u7406\u6587\u4EF6\u548C\u76EE\
  \u5F55\u3002\u8981\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728\uFF0C\u9996\u5148\
  \u9700\u8981\u521D\u59CB\u5316SD\u5361\uFF0C\u7136\u540E\u4F7F\u7528SD\u5E93\u4E2D\
  \u7684`exists()`\u65B9\u6CD5\u3002 \u9996\u5148\uFF0C\u5305\u542BSD\u5E93\u5E76\u58F0\
  \u660E\u82AF\u7247\u9009\u62E9\u5F15\u811A\uFF1A."
lastmod: '2024-03-13T22:44:48.078811-06:00'
model: gpt-4-0125-preview
summary: "Arduino\u9ED8\u8BA4\u4E0D\u652F\u6301\u590D\u6742\u7684\u6587\u4EF6\u7CFB\
  \u7EDF\u64CD\u4F5C\u3002\u4F46\u662F\uFF0C\u901A\u8FC7\u4F7F\u7528SD\u5E93\uFF0C\
  \u5B83\u662F\u6807\u51C6Arduino IDE\u7684\u4E00\u90E8\u5206\uFF0C\u60A8\u53EF\u4EE5\
  \u8F7B\u677E\u5730\u5904\u7406\u6587\u4EF6\u548C\u76EE\u5F55\u3002\u8981\u68C0\u67E5\
  \u76EE\u5F55\u662F\u5426\u5B58\u5728\uFF0C\u9996\u5148\u9700\u8981\u521D\u59CB\u5316\
  SD\u5361\uFF0C\u7136\u540E\u4F7F\u7528SD\u5E93\u4E2D\u7684`exists()`\u65B9\u6CD5\
  ."
title: "\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728"
weight: 20
---

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
