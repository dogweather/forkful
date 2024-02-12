---
title:                "获取字符串的长度"
aliases:
- /zh/arduino/finding-the-length-of-a-string.md
date:                  2024-01-20T17:46:47.494508-07:00
model:                 gpt-4-1106-preview
simple_title:         "获取字符串的长度"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (是什么以及为什么？)
在Arduino编程中，确定字符串长度就是找出字符串中有多少字符。这对于内存管理和性能优化来说非常重要。

## How to? (如何做？)
```Arduino
void setup() {
  Serial.begin(9600); // 开始串行通信
  String greeting = "你好，世界！";
  int length = greeting.length(); // 获取字符串长度
  Serial.println(length); // 打印长度
}

void loop() {
  // 这里不需要循环代码
}
```
输出:
```
6
```

请注意，输出可能会让你感到困惑，因为中文字符可能被视为多个字符。

## Deep Dive (深入了解)
获取字符串长度这功能，从C语言的`strlen()`函数继承而来，曾是处理C字符串的通用方式。在Arduino中，`String`对象的`.length()`方法提供了这一功能，它更为安全且容易使用。虽然`strlen()`是测量字符数组长度的快速方法，但这涉及指针操作，对初学者来说可能较为复杂，而且如果没有正确地使用结束字符'\0'，则可能引起错误。Arduino的`String`类封装了这些细节，让字符串操作变得更加简单和安全。使用`.length()`不需要担心字符串结尾或内存溢出问题。

### Alternatives (替代方案)
如果你使用的是字符数组（C风格字符串），你可以使用`strlen()`函数。但需确保你的字符数组以空字符'\0'结尾。

```Arduino
#include <string.h>  // 为了使用strlen()

void setup() {
  Serial.begin(9600);
  char message[] = "Arduino 字符串";
  Serial.println(strlen(message));  // 打印字符串长度
}

void loop() {
  // 这里不需要循环代码
}
```

### Implementation Details (实现细节)
在编写使用字符串的程序时，理解字符串是如何存储在内存中的非常重要。Arduino的`String`对象动态管理内存，意味着它会根据需要增加或减少内存分配。相比之下，字符数组的大小在编译时就已固定，不具备动态调整能力。

## See Also (另请参见)
- Arduino官方文档中的`String`类：[Arduino String Reference](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- 关于C字符串和`strlen()`函数的更多信息：[C String Handling](https://www.cplusplus.com/reference/cstring/)
