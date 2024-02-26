---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:01.433186-07:00
description: "\u5728 Arduino \u7F16\u7A0B\u4E2D\u5411\u6807\u51C6\u9519\u8BEF(stderr)\u5199\
  \u5165\u5185\u5BB9\u6D89\u53CA\u5C06\u9519\u8BEF\u6D88\u606F\u548C\u8BCA\u65AD\u4FE1\
  \u606F\u5F15\u5BFC\u5230\u4E00\u4E2A\u72EC\u7ACB\u7684\u901A\u9053\uFF0C\u786E\u4FDD\
  \u5B83\u4EEC\u4E0D\u4E0E\u6807\u51C6\u8F93\u51FA(stdout)\u6DF7\u5408\u3002\u7A0B\
  \u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u533A\u5206\u6B63\u5E38\u7A0B\u5E8F\
  \u8F93\u51FA\u4E0E\u9519\u8BEF\u6D88\u606F\uFF0C\u4F7F\u5F97\u8C03\u8BD5\u548C\u65E5\
  \u5FD7\u5206\u6790\u66F4\u52A0\u76F4\u63A5\u3002"
lastmod: '2024-02-25T18:49:45.647417-07:00'
model: gpt-4-0125-preview
summary: "\u5728 Arduino \u7F16\u7A0B\u4E2D\u5411\u6807\u51C6\u9519\u8BEF(stderr)\u5199\
  \u5165\u5185\u5BB9\u6D89\u53CA\u5C06\u9519\u8BEF\u6D88\u606F\u548C\u8BCA\u65AD\u4FE1\
  \u606F\u5F15\u5BFC\u5230\u4E00\u4E2A\u72EC\u7ACB\u7684\u901A\u9053\uFF0C\u786E\u4FDD\
  \u5B83\u4EEC\u4E0D\u4E0E\u6807\u51C6\u8F93\u51FA(stdout)\u6DF7\u5408\u3002\u7A0B\
  \u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u533A\u5206\u6B63\u5E38\u7A0B\u5E8F\
  \u8F93\u51FA\u4E0E\u9519\u8BEF\u6D88\u606F\uFF0C\u4F7F\u5F97\u8C03\u8BD5\u548C\u65E5\
  \u5FD7\u5206\u6790\u66F4\u52A0\u76F4\u63A5\u3002"
title: "\u5199\u5165\u6807\u51C6\u9519\u8BEF"
---

{{< edit_this_page >}}

## 什么 & 为什么?

在 Arduino 编程中向标准错误(stderr)写入内容涉及将错误消息和诊断信息引导到一个独立的通道，确保它们不与标准输出(stdout)混合。程序员这样做是为了区分正常程序输出与错误消息，使得调试和日志分析更加直接。

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
