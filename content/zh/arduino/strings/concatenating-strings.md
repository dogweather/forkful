---
date: 2024-01-20 17:33:54.603255-07:00
description: "How to (\u600E\u4E48\u505A) \u8FC7\u53BB\uFF0CArduino\u4E0A\u7684\u5B57\
  \u7B26\u4E32\u64CD\u4F5C\u4E3B\u8981\u4F9D\u8D56\u4E8E\u5B57\u7B26\u6570\u7EC4\u548C\
  C\u6807\u51C6\u5E93\u51FD\u6570\u3002\u73B0\u5728\uFF0C`String` \u7C7B\u63D0\u4F9B\
  \u4E86\u4E00\u4E2A\u66F4\u5BB9\u6613\u4F7F\u7528\u7684\u9009\u62E9\u3002\u4F46\u8BB0\
  \u4F4F\uFF0C\u8FC7\u5EA6\u4F7F\u7528 `String` \u53EF\u80FD\u4F1A\u5BFC\u81F4\u5185\
  \u5B58\u788E\u7247\u3002\u66FF\u4EE3\u65B9\u6CD5\u5305\u62EC\u4F7F\u7528 `strcat`,\
  \ `strcpy`, \u548C C++\u4E2D\u7684\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:48.348525-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u5B57\u7B26\u4E32\u62FC\u63A5"
weight: 3
---

## How to (怎么做)
```Arduino
void setup() {
  Serial.begin(9600);

  String firstName = "Li";
  String lastName = "Ming";
  String fullName = firstName + " " + lastName; // 串联字符串

  Serial.println(fullName); // 打印：Li Ming
}

void loop() {
  // 这里什么都不做
}
```

## Deep Dive (深入探讨)
过去，Arduino上的字符串操作主要依赖于字符数组和C标准库函数。现在，`String` 类提供了一个更容易使用的选择。但记住，过度使用 `String` 可能会导致内存碎片。替代方法包括使用 `strcat`, `strcpy`, 和 C++中的 `stringstream`。在重复或复杂的串联时，考虑内存和性能是很有必要的。

## See Also (另请参见)
- [Arduino String Reference](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [Arduino Memory](https://www.arduino.cc/en/Tutorial/Foundations/Memory)
- [C++ String Streams](http://www.cplusplus.com/reference/sstream/)
