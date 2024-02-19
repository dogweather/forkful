---
aliases:
- /zh/arduino/writing-a-text-file/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:53.550265-07:00
description: "\u5728Arduino\u4E2D\u5199\u5165\u6587\u672C\u6587\u4EF6\u6D89\u53CA\u5230\
  \u5728SD\u5361\u6216\u7C7B\u4F3C\u5B58\u50A8\u6A21\u5757\u4E0A\u4FDD\u5B58\u6570\
  \u636E\uFF0C\u901A\u5E38\u7528\u4E8E\u6570\u636E\u8BB0\u5F55\u76EE\u7684\u3002\u7A0B\
  \u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u8BB0\u5F55\u4F20\u611F\u5668\u8BFB\
  \u6570\u3001\u4FDD\u5B58\u914D\u7F6E\u6216\u968F\u65F6\u95F4\u8BB0\u5F55\u5E94\u7528\
  \u4E8B\u4EF6\uFF0C\u8FD9\u5BF9\u4E8E\u9700\u8981\u6570\u636E\u5206\u6790\u6216\u8DDF\
  \u8E2A\u7684\u9879\u76EE\u81F3\u5173\u91CD\u8981\u3002"
lastmod: 2024-02-18 23:08:59.385074
model: gpt-4-0125-preview
summary: "\u5728Arduino\u4E2D\u5199\u5165\u6587\u672C\u6587\u4EF6\u6D89\u53CA\u5230\
  \u5728SD\u5361\u6216\u7C7B\u4F3C\u5B58\u50A8\u6A21\u5757\u4E0A\u4FDD\u5B58\u6570\
  \u636E\uFF0C\u901A\u5E38\u7528\u4E8E\u6570\u636E\u8BB0\u5F55\u76EE\u7684\u3002\u7A0B\
  \u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u8BB0\u5F55\u4F20\u611F\u5668\u8BFB\
  \u6570\u3001\u4FDD\u5B58\u914D\u7F6E\u6216\u968F\u65F6\u95F4\u8BB0\u5F55\u5E94\u7528\
  \u4E8B\u4EF6\uFF0C\u8FD9\u5BF9\u4E8E\u9700\u8981\u6570\u636E\u5206\u6790\u6216\u8DDF\
  \u8E2A\u7684\u9879\u76EE\u81F3\u5173\u91CD\u8981\u3002"
title: "\u7F16\u5199\u6587\u672C\u6587\u4EF6"
---

{{< edit_this_page >}}

## 什么和为什么？
在Arduino中写入文本文件涉及到在SD卡或类似存储模块上保存数据，通常用于数据记录目的。程序员这样做是为了记录传感器读数、保存配置或随时间记录应用事件，这对于需要数据分析或跟踪的项目至关重要。

## 如何操作：
要使用Arduino在SD卡上写入文本文件，你首先需要包含`SD.h`库，该库提供与SD卡交互所需的函数。确保你的Arduino板连接到了SD卡模块。

```cpp
#include <SPI.h>
#include <SD.h>

File myFile;

void setup() {
  // 以每秒9600位的速度初始化串行通信：
  Serial.begin(9600);
  
  // 检查SD卡初始化
  if (!SD.begin(4)) {
    Serial.println("初始化失败！");
    return;
  }
  Serial.println("初始化完成。");
  
  // 打开文件。注意，每次只能打开一个文件，
  // 所以在打开另一个之前你必须关闭这个。
  myFile = SD.open("test.txt", FILE_WRITE);
  
  // 如果文件成功打开，写入它：
  if (myFile) {
    Serial.print("正在写入test.txt...");
    myFile.println("测试文本文件写入。");
    // 关闭文件：
    myFile.close();
    Serial.println("完成。");
  } else {
    // 如果文件没打开，打印一个错误：
    Serial.println("打开test.txt失败");
  }
}

void loop() {
  // setup之后不会发生任何事情
}
```

### 样本输出：
当你运行这段代码时，Arduino IDE串行监视器将显示：
```
初始化完成。
正在写入test.txt...完成。
```
要检查数据是否正确写入，你可以从Arduino中拿出SD卡，插入电脑，并打开`test.txt`文件查看消息"测试文本文件写入。"

对于需要更高级文件操作或处理的项目，考虑探索额外的库或编写定制函数以满足你的特定需求。
