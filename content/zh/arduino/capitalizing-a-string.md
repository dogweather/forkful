---
title:                "字符串首字母大写"
date:                  2024-01-19
html_title:           "Arduino: 字符串首字母大写"
simple_title:         "字符串首字母大写"

category:             "Arduino"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
大写转换是将字符串中的所有字母改为大写形式。程序员这么做通常是为了标准化文本输入，或创建视觉上的突出显示。

## How to: (如何实现：)
```Arduino
void setup() {
  Serial.begin(9600);
  String message = "Hello, World!";
  message.toUpperCase();
  Serial.println(message);  // 输出: HELLO, WORLD!
}

void loop() {
  // 这里不需要代码，因为大写转换只在setup()中执行一次。
}
```

## Deep Dive (深入了解)
在早期编程语言中，大写转换往往更为复杂，需要对每个字符手动处理。在 Arduino 中，`String` 类为这提供了 `toUpperCase()` 方法，简化了转换。但有一点，使用 `String` 类可能会导致内存碎片化。如果内存管理是关键，可以考虑用字符数组和`strlwr()`函数替代。

## See Also (另请参阅)
- Arduino `String` 类参考: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- ASCII表和字符编码：https://www.arduino.cc/reference/en/language/functions/communication/serial/print/
- 关于内存管理：https://www.arduino.cc/en/Tutorial/Memory
