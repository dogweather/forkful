---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:53.550265-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u8981\u4F7F\u7528Arduino\u5728SD\u5361\
  \u4E0A\u5199\u5165\u6587\u672C\u6587\u4EF6\uFF0C\u4F60\u9996\u5148\u9700\u8981\u5305\
  \u542B`SD.h`\u5E93\uFF0C\u8BE5\u5E93\u63D0\u4F9B\u4E0ESD\u5361\u4EA4\u4E92\u6240\
  \u9700\u7684\u51FD\u6570\u3002\u786E\u4FDD\u4F60\u7684Arduino\u677F\u8FDE\u63A5\u5230\
  \u4E86SD\u5361\u6A21\u5757\u3002"
lastmod: '2024-03-13T22:44:48.083899-06:00'
model: gpt-4-0125-preview
summary: "\u8981\u4F7F\u7528Arduino\u5728SD\u5361\u4E0A\u5199\u5165\u6587\u672C\u6587\
  \u4EF6\uFF0C\u4F60\u9996\u5148\u9700\u8981\u5305\u542B`SD.h`\u5E93\uFF0C\u8BE5\u5E93\
  \u63D0\u4F9B\u4E0ESD\u5361\u4EA4\u4E92\u6240\u9700\u7684\u51FD\u6570\u3002\u786E\
  \u4FDD\u4F60\u7684Arduino\u677F\u8FDE\u63A5\u5230\u4E86SD\u5361\u6A21\u5757."
title: "\u7F16\u5199\u6587\u672C\u6587\u4EF6"
weight: 24
---

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
