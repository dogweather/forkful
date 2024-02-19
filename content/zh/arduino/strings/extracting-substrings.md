---
aliases:
- /zh/arduino/extracting-substrings/
date: 2024-01-20 17:45:06.067106-07:00
description: "\u63D0\u53D6\u5B50\u4E32\u662F\u6307\u4ECE\u4E00\u4E2A\u5B57\u7B26\u4E32\
  \u4E2D\u83B7\u53D6\u4E00\u90E8\u5206\u5185\u5BB9\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\
  \u505A\u6765\u5206\u6790\u3001\u5904\u7406\u6216\u6539\u53D8\u6570\u636E\uFF0C\u4EE5\
  \u6EE1\u8DB3\u7279\u5B9A\u9700\u6C42\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:59.359897
model: gpt-4-1106-preview
summary: "\u63D0\u53D6\u5B50\u4E32\u662F\u6307\u4ECE\u4E00\u4E2A\u5B57\u7B26\u4E32\
  \u4E2D\u83B7\u53D6\u4E00\u90E8\u5206\u5185\u5BB9\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\
  \u505A\u6765\u5206\u6790\u3001\u5904\u7406\u6216\u6539\u53D8\u6570\u636E\uFF0C\u4EE5\
  \u6EE1\u8DB3\u7279\u5B9A\u9700\u6C42\u3002"
title: "\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32"
---

{{< edit_this_page >}}

## What & Why? (是什么 & 为什么?)
提取子串是指从一个字符串中获取一部分内容。程序员这样做来分析、处理或改变数据，以满足特定需求。

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
