---
title:                "使用正则表达式"
date:                  2024-01-19
simple_title:         "使用正则表达式"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (是什么以及为什么？)
正则表达式是字符串匹配的强有力工具，用于高效查找、替换文本。程序员使用它们进行复杂文本分析，简化字符串操作。

## How to: (如何操作：)
Arduino 本身不直接支持正则表达式，但你可以使用标准的字符串函数或通过外部库来处理。下面是简单字符串搜索的例子：

```arduino
#include <Arduino.h>

void setup() {
  Serial.begin(9600);
  // 搜索的字符串
  String haystack = "Arduino123";
  // 要搜索的内容
  String needle = "123";
  
  if (haystack.indexOf(needle) > -1) {
    Serial.println("找到了!");
  } else {
    Serial.println("没找到.");
  }
}

void loop() {
  // 无需循环代码
}
```

输出样例：
```
找到了!
```

## Deep Dive (深入探讨)
- Arduino 不内置正则表达式功能，因为它设计用于更直接、资源受限的嵌入式系统中。
- 替代方案: 可以使用 `String` 类的 `indexOf()`, `lastIndexOf()`, `startsWith()`, `endsWith()` 等方法进行简单文本搜索。
- 实现细节: 第三方库如 `regex.h` 可能提供额外正则表达式支持，但要注意内存和处理速度限制。

## See Also (参考链接)
- Arduino String 类参考：[https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- Arduino 第三方正则表达式库：搜索在线 Arduino 库资源。
