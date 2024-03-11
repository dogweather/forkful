---
date: 2024-01-20 17:33:54.603255-07:00
description: "\u5728\u7F16\u7A0B\u4E2D\uFF0C\u4E32\u8054\u5B57\u7B26\u4E32\u5C31\u662F\
  \u628A\u4E24\u4E2A\u6216\u591A\u4E2A\u5B57\u7B26\u4E32\u62FC\u63A5\u6210\u4E00\u4E2A\
  \u3002\u4E3A\u4EC0\u4E48\u8981\u8FD9\u4E48\u505A\uFF1F\u56E0\u4E3A\u6211\u4EEC\u7ECF\
  \u5E38\u9700\u8981\u6574\u5408\u4FE1\u606F\uFF0C\u6BD4\u5982\u663E\u793A\u5B8C\u6574\
  \u7684\u6D88\u606F\u6216\u521B\u5EFA\u52A8\u6001\u5185\u5BB9\u3002"
isCJKLanguage: true
lastmod: '2024-03-11T00:14:21.852393-06:00'
model: gpt-4-1106-preview
summary: "\u5728\u7F16\u7A0B\u4E2D\uFF0C\u4E32\u8054\u5B57\u7B26\u4E32\u5C31\u662F\
  \u628A\u4E24\u4E2A\u6216\u591A\u4E2A\u5B57\u7B26\u4E32\u62FC\u63A5\u6210\u4E00\u4E2A\
  \u3002\u4E3A\u4EC0\u4E48\u8981\u8FD9\u4E48\u505A\uFF1F\u56E0\u4E3A\u6211\u4EEC\u7ECF\
  \u5E38\u9700\u8981\u6574\u5408\u4FE1\u606F\uFF0C\u6BD4\u5982\u663E\u793A\u5B8C\u6574\
  \u7684\u6D88\u606F\u6216\u521B\u5EFA\u52A8\u6001\u5185\u5BB9\u3002"
title: "\u5B57\u7B26\u4E32\u62FC\u63A5"
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
