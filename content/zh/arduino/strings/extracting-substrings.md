---
date: 2024-01-20 17:45:06.067106-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C) \u5728Arduino\u4E2D\u6709\u51E0\u79CD\
  \u65B9\u5F0F\u63D0\u53D6\u5B50\u4E32\u3002\u4E0B\u9762\u662F\u4E24\u79CD\u5E38\u7528\
  \u65B9\u6CD5\u7684\u793A\u4F8B\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:48.345228-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u64CD\u4F5C) \u5728Arduino\u4E2D\u6709\u51E0\u79CD\u65B9\u5F0F\
  \u63D0\u53D6\u5B50\u4E32\u3002\u4E0B\u9762\u662F\u4E24\u79CD\u5E38\u7528\u65B9\u6CD5\
  \u7684\u793A\u4F8B\u3002"
title: "\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32"
weight: 6
---

## How to: (如何操作)
在Arduino中有几种方式提取子串。下面是两种常用方法的示例。

```arduino
String str = "你好，世界！";
String sub1 = str.substring(3); // 从第4个字符开始到末尾
String sub2 = str.substring(3, 5); // 从第4个到第5个字符

void setup() {
    Serial.begin(9600);
    while (!Serial) {
        ; // 等待串行端口连接。
    }
    
    Serial.println(sub1); // 输出：世界！
    Serial.println(sub2); // 输出：世
}

void loop() {
    // 这里不需要代码。
}
```

## Deep Dive (深入了解)
提取子串功能在Arduino编程中不是新鲜事。它来源于更早的编程语言，如C和Java，它们极大影响了Arduino语言设计。另外的方法有字符数组处理、指针操作，但在Arduino中，字符串类提供了更简单的操作方式。当提取子串时，要注意内存管理以避免溢出和泄漏。

## See Also (另请参阅)
- Arduino String Reference: [https://www.arduino.cc/reference/en/language/variables/data-types/string/](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- Arduino Forum, String manipulation: [https://forum.arduino.cc/index.php?board=9.0](https://forum.arduino.cc/index.php?board=9.0)
