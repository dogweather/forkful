---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:02.888296-07:00
description: "\u5982\u4F55\u5B9E\u73B0\uFF1A Arduino\uFF0C\u4E3B\u8981\u7528\u4E8E\
  \u4E0E\u786C\u4EF6\u4EA4\u4E92\uFF0C\u4E5F\u901A\u8FC7\u5176 `String` \u5BF9\u8C61\
  \u5305\u542B\u4E86\u57FA\u672C\u7684\u5B57\u7B26\u4E32\u64CD\u4F5C\u80FD\u529B\u3002\
  \u7136\u800C\uFF0C\u5B83\u7F3A\u4E4F\u5728\u9AD8\u7EA7\u8BED\u8A00\u4E2D\u770B\u5230\
  \u7684\u76F4\u63A5\u7684 `capitalize` \u51FD\u6570\u3002\u56E0\u6B64\uFF0C\u6211\
  \u4EEC\u901A\u8FC7\u8FED\u4EE3\u5B57\u7B26\u4E32\u5E76\u5E94\u7528\u5927\u5C0F\u5199\
  \u8F6C\u6362\u6765\u5B9E\u73B0\u9996\u5B57\u6BCD\u5927\u5199\u3002 \u8FD9\u91CC\u6709\
  \u4E00\u4E2A\u4E0D\u4F7F\u7528\u7B2C\u4E09\u65B9\u5E93\u7684\u57FA\u672C\u793A\u4F8B\
  \uFF1A."
lastmod: '2024-03-13T22:44:48.044184-06:00'
model: gpt-4-0125-preview
summary: "Arduino\uFF0C\u4E3B\u8981\u7528\u4E8E\u4E0E\u786C\u4EF6\u4EA4\u4E92\uFF0C\
  \u4E5F\u901A\u8FC7\u5176 `String` \u5BF9\u8C61\u5305\u542B\u4E86\u57FA\u672C\u7684\
  \u5B57\u7B26\u4E32\u64CD\u4F5C\u80FD\u529B\u3002\u7136\u800C\uFF0C\u5B83\u7F3A\u4E4F\
  \u5728\u9AD8\u7EA7\u8BED\u8A00\u4E2D\u770B\u5230\u7684\u76F4\u63A5\u7684 `capitalize`\
  \ \u51FD\u6570\u3002\u56E0\u6B64\uFF0C\u6211\u4EEC\u901A\u8FC7\u8FED\u4EE3\u5B57\
  \u7B26\u4E32\u5E76\u5E94\u7528\u5927\u5C0F\u5199\u8F6C\u6362\u6765\u5B9E\u73B0\u9996\
  \u5B57\u6BCD\u5927\u5199."
title: "\u5B57\u7B26\u4E32\u5927\u5199\u5316"
weight: 2
---

## 如何实现：
Arduino，主要用于与硬件交互，也通过其 `String` 对象包含了基本的字符串操作能力。然而，它缺乏在高级语言中看到的直接的 `capitalize` 函数。因此，我们通过迭代字符串并应用大小写转换来实现首字母大写。

这里有一个不使用第三方库的基本示例：

```cpp
String capitalizeString(String input) {
  if (input.length() == 0) {
    return ""; // 如果输入为空，则返回一个空字符串
  }
  input.toLowerCase(); // 首先将整个字符串转换为小写
  input.setCharAt(0, input.charAt(0) - 32); // 将第一个字符转换为大写
  
  // 将跟随空格后的字母转换为大写
  for (int i = 1; i < input.length(); i++) {
    if (input.charAt(i - 1) == ' ') {
      input.setCharAt(i, input.charAt(i) - 32);
    }
  }
  return input;
}

void setup() {
  Serial.begin(9600);
  String testStr = "hello arduino world";
  String capitalizedStr = capitalizeString(testStr);
  Serial.println(capitalizedStr); // 输出： "Hello Arduino World"
}

void loop() {
  // 空循环
}
```

这段代码片段定义了一个 `capitalizeString` 函数，首先将整个字符串转换为小写以标准化其大小写。然后，它将第一个字符和任何跟随空格的字符都转换为大写，有效地将输入字符串中的每个单词首字母大写。请注意，这个初级实现假设 ASCII 字符编码，并可能需要调整以完全支持 Unicode。

目前，由于Arduino生态系统主要关注硬件交互和效率，因此尚无广泛采用的专门用于字符串操作的第三方库。然而，提供的示例是在Arduino编程环境内实现字符串首字母大写的一种简单方法。
