---
date: 2024-01-26 00:59:24.655116-07:00
description: "\u5982\u4F55\u5B9E\u65BD\uFF1A Arduino\u5E76\u6CA1\u6709\u50CF\u5176\
  \u4ED6\u4E00\u4E9B\u73AF\u5883\u90A3\u6837\u5185\u7F6E\u65E5\u5FD7\u8BB0\u5F55\u5E93\
  \uFF0C\u4F46\u4F60\u53EF\u4EE5\u7528\u6700\u5C11\u7684\u9EBB\u70E6\u5728Serial\u63A7\
  \u5236\u53F0\u5B9E\u73B0\u57FA\u672C\u7684\u65E5\u5FD7\u8BB0\u5F55\u3002\u4EE5\u4E0B\
  \u662F\u4E00\u4E2A\u5FEB\u901F\u5165\u95E8\u793A\u4F8B\uFF1A."
lastmod: '2024-04-05T21:53:48.365068-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u65E5\u5FD7\u8BB0\u5F55"
weight: 17
---

## 如何实施：
Arduino并没有像其他一些环境那样内置日志记录库，但你可以用最少的麻烦在Serial控制台实现基本的日志记录。以下是一个快速入门示例：

```arduino
void setup() {
  // 以给定的波特率启动串行通信
  Serial.begin(9600);

  // 等待串行端口连接 - 仅在某些板上需要
  while (!Serial) {
    ; // 等待串行端口连接。对于本机USB来说这是必需的
  }

  // 记录一条信息性消息，表示设置过程已完成
  Serial.println("Setup complete!");
}

void loop() {
  // 简单的记录器，每秒打印一次运行时间
  static unsigned long lastLogTime = 0;
  unsigned long currentMillis = millis();

  if (currentMillis - lastLogTime >= 1000) {
    lastLogTime = currentMillis;
    Serial.print("Uptime (ms): ");
    Serial.println(currentMillis);

    // 在这里，你还可以添加错误日志、警告或其他信息。
  }
  
  // 在这里放置你程序的其他逻辑...
}
```

串行输出样本：
```
Setup complete!
Uptime (ms): 1000
Uptime (ms): 2000
Uptime (ms): 3000
...
```

## 深入了解：
从历史上看，微控制器上的日志记录不像在功能齐全的操作系统上那么简单。有限的资源意味着每个字节都很重要，开发者需要小心不要阻塞系统。随着功能更强大的板卡的出现以及Arduino平台简化了过程，日志记录变得更加可行。

虽然上述代码演示了通过Serial接口进行日志记录，但其他方法包括写入SD卡、通过网络向远程服务器发送数据，甚至输出到小型LCD。

实施日志记录系统需要考虑诸如轮换、严重性级别（信息、调试、警告、错误）和性能影响等问题。在Arduino上，当记录复杂的数据结构时，你可能需要注意内存限制。对于远程日志记录，传输日志的安全性也是一个关注点。

在Arduino世界之外，存在更复杂的解决方案，如Syslog（一个被广泛采用的日志记录标准），但你可以集成第三方库，这些库提供了类似的功能，有着不同程度的复杂性和资源要求。

## 另见：
- [Arduino `Serial` 参考资料](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [使用Arduino进行SD卡日志记录](https://www.arduino.cc/en/Tutorial/LibraryExamples/Datalogger)
- [SparkFun的数据记录盾](https://www.sparkfun.com/products/13712)
- [TinyWeb：使用Arduino进行远程日志记录的实际示例](https://www.arduino.cc/en/Tutorial/WebClientRepeating)
