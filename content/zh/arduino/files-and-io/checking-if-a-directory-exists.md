---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:39.583938-07:00
description: "\u5728Arduino\u7F16\u7A0B\u7684\u80CC\u666F\u4E0B\uFF0C\u68C0\u67E5\
  SD\u5361\u6216\u7C7B\u4F3C\u5B58\u50A8\u6A21\u5757\u4E0A\u662F\u5426\u5B58\u5728\
  \u76EE\u5F55\uFF0C\u5141\u8BB8\u60A8\u5728\u4E0D\u51FA\u9519\u7684\u60C5\u51B5\u4E0B\
  \u8BFB\u53D6\u6216\u5199\u5165\u6587\u4EF6\u3002\u8FD9\u4E2A\u64CD\u4F5C\u5BF9\u4E8E\
  \u6570\u636E\u8BB0\u5F55\u3001\u914D\u7F6E\u7BA1\u7406\u6216\u4EFB\u4F55\u9700\u8981\
  \u7ED3\u6784\u5316\u6587\u4EF6\u5B58\u50A8\u7684\u4EFB\u52A1\u81F3\u5173\u91CD\u8981\
  \uFF0C\u786E\u4FDD\u4E86\u5E94\u7528\u7A0B\u5E8F\u4E2D\u7684\u53EF\u9760\u6027\u548C\
  \u6D41\u7545\u6027\u80FD\u3002"
lastmod: '2024-03-13T22:44:48.078811-06:00'
model: gpt-4-0125-preview
summary: "\u5728Arduino\u7F16\u7A0B\u7684\u80CC\u666F\u4E0B\uFF0C\u68C0\u67E5SD\u5361\
  \u6216\u7C7B\u4F3C\u5B58\u50A8\u6A21\u5757\u4E0A\u662F\u5426\u5B58\u5728\u76EE\u5F55\
  \uFF0C\u5141\u8BB8\u60A8\u5728\u4E0D\u51FA\u9519\u7684\u60C5\u51B5\u4E0B\u8BFB\u53D6\
  \u6216\u5199\u5165\u6587\u4EF6\u3002\u8FD9\u4E2A\u64CD\u4F5C\u5BF9\u4E8E\u6570\u636E\
  \u8BB0\u5F55\u3001\u914D\u7F6E\u7BA1\u7406\u6216\u4EFB\u4F55\u9700\u8981\u7ED3\u6784\
  \u5316\u6587\u4EF6\u5B58\u50A8\u7684\u4EFB\u52A1\u81F3\u5173\u91CD\u8981\uFF0C\u786E\
  \u4FDD\u4E86\u5E94\u7528\u7A0B\u5E8F\u4E2D\u7684\u53EF\u9760\u6027\u548C\u6D41\u7545\
  \u6027\u80FD\u3002"
title: "\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728"
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
