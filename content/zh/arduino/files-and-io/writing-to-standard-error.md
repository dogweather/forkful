---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:01.433186-07:00
description: "\u5982\u4F55\u64CD\u4F5C: Arduino \u539F\u751F\u5E76\u4E0D\u50CF\u4F20\
  \u7EDF\u8BA1\u7B97\u7CFB\u7EDF\u90A3\u6837\u533A\u5206\u6807\u51C6\u8F93\u51FA\u548C\
  \u6807\u51C6\u9519\u8BEF\u3002`Serial.print()` \u548C `Serial.println()` \u65B9\u6CD5\
  \u90FD\u5199\u5165\u76F8\u540C\u7684\u4E32\u884C\u8F93\u51FA\uFF0C\u901A\u5E38\u5728\
  \ Arduino IDE \u7684\u4E32\u884C\u76D1\u89C6\u5668\u4E2D\u67E5\u770B\u3002\u7136\
  \u800C\uFF0C\u6211\u4EEC\u53EF\u4EE5\u901A\u8FC7\u7279\u5B9A\u683C\u5F0F\u5316\u9519\
  \u8BEF\u6D88\u606F\u6216\u5C06\u5B83\u4EEC\u5F15\u5BFC\u5230\u4E00\u4E2A\u66FF\u4EE3\
  \u8F93\u51FA\uFF08\u4F8B\u5982 SD\u2026"
lastmod: '2024-03-13T22:44:48.081319-06:00'
model: gpt-4-0125-preview
summary: "Arduino \u539F\u751F\u5E76\u4E0D\u50CF\u4F20\u7EDF\u8BA1\u7B97\u7CFB\u7EDF\
  \u90A3\u6837\u533A\u5206\u6807\u51C6\u8F93\u51FA\u548C\u6807\u51C6\u9519\u8BEF\u3002\
  `Serial.print()` \u548C `Serial.println()` \u65B9\u6CD5\u90FD\u5199\u5165\u76F8\u540C\
  \u7684\u4E32\u884C\u8F93\u51FA\uFF0C\u901A\u5E38\u5728 Arduino IDE \u7684\u4E32\u884C\
  \u76D1\u89C6\u5668\u4E2D\u67E5\u770B\u3002\u7136\u800C\uFF0C\u6211\u4EEC\u53EF\u4EE5\
  \u901A\u8FC7\u7279\u5B9A\u683C\u5F0F\u5316\u9519\u8BEF\u6D88\u606F\u6216\u5C06\u5B83\
  \u4EEC\u5F15\u5BFC\u5230\u4E00\u4E2A\u66FF\u4EE3\u8F93\u51FA\uFF08\u4F8B\u5982 SD\
  \ \u5361\u4E0A\u7684\u6587\u4EF6\u6216\u901A\u8FC7\u7F51\u7EDC\u8FDE\u63A5\uFF09\
  \u6765\u6A21\u62DF\u5199\u5165 stderr."
title: "\u5199\u5165\u6807\u51C6\u9519\u8BEF"
weight: 25
---

## 如何操作:
Arduino 原生并不像传统计算系统那样区分标准输出和标准错误。`Serial.print()` 和 `Serial.println()` 方法都写入相同的串行输出，通常在 Arduino IDE 的串行监视器中查看。然而，我们可以通过特定格式化错误消息或将它们引导到一个替代输出（例如 SD 卡上的文件或通过网络连接）来模拟写入 stderr。

为了模拟 stderr，你可以在错误消息前加上像 "ERROR:" 这样的标签来区分它们在串行监视器中的显示：

```cpp
void setup() {
  Serial.begin(9600); // 初始化串行通信，波特率为 9600
}

void loop() {
  int result = someFunction();
  if (result == -1) {
    // 通过在错误消息前添加前缀来模拟 stderr
    Serial.println("ERROR: 函数执行失败。");
  } else {
    Serial.println("函数成功执行。");
  }
  delay(1000); // 循环前等待一秒
}

int someFunction() {
  // 一个在出错时返回 -1 的虚拟函数
  return -1;
}
```

在 Arduino IDE 的串行监视器中，示例输出可能如下所示：

```
ERROR: 函数执行失败。
```

对于需要更复杂方式的项目，包括写入到不同的物理输出，可能需要使用第三方库或额外的硬件。例如，将错误消息记录到 SD 卡需要使用 `SD` 库：

```cpp
#include <SPI.h>
#include <SD.h>

File myFile;

void setup() {
  Serial.begin(9600);
  if (!SD.begin()) {
    Serial.println("ERROR: SD卡初始化失败！");
    return;
  }
  
  myFile = SD.open("error.log", FILE_WRITE);
  if (myFile) {
    myFile.println("ERROR: 函数执行失败。");
    myFile.close(); // 确保关闭文件以保存内容
  } else {
    Serial.println("ERROR: 打开 error.log 失败！");
  }
}

void loop() {
  // 你的主要代码将放在这里
}
```

通过这种方式，你可以通过将错误消息引导到 SD 卡上的 `error.log` 文件来物理地分离正常程序输出和错误消息，使得在不干扰主要输出通道的情况下启用事后分析。
