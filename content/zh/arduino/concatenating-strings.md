---
title:                "字符串拼接"
aliases:
- zh/arduino/concatenating-strings.md
date:                  2024-01-20T17:33:54.603255-07:00
model:                 gpt-4-1106-preview
simple_title:         "字符串拼接"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)

在编程中，串联字符串就是把两个或多个字符串拼接成一个。为什么要这么做？因为我们经常需要整合信息，比如显示完整的消息或创建动态内容。

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
