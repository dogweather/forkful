---
title:                "字符串大写化"
date:                  2024-02-03T19:05:02.888296-07:00
model:                 gpt-4-0125-preview
simple_title:         "字符串大写化"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么？
将字符串中每个单词的第一个字符转换为大写，同时确保其余字符保持小写，这个操作称为字符串首字母大写。这种操作在数据格式化和用户输入标准化中很常见，以保持一致性和提高可读性。

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
