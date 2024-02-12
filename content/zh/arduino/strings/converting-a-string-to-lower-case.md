---
title:                "将字符串转换为小写"
aliases:
- /zh/arduino/converting-a-string-to-lower-case.md
date:                  2024-01-20T17:38:10.910041-07:00
model:                 gpt-4-1106-preview
simple_title:         "将字符串转换为小写"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
字符串转换为小写，就是将所有大写字母变为小写。这在处理用户输入或做不区分大小写的比较时很有用。

## 如何：
```Arduino
String original = "Hello, Arduino!";
String lowerCase = original.toLowerCase();

void setup() {
  Serial.begin(9600);
}

void loop() {
  Serial.println(lowerCase);
  delay(1000); // Wait for 1 second before repeating
}
```
输出：
```
hello, arduino!
```

## 深入了解
字符串转换成小写这个操作存在已久，因为它是文本处理中的基本功能。除了`String`类的`toLowerCase()`方法外，还可以通过遍历字符串中的每个字符，利用ASCII码表来手动转换。不过，Arduino提供的方法更易用，效率也更高，它隐藏了实现细节。内部实现机制通常涉及检查字符是否在大写字母的ASCII范围内（65-90），如果是，就通过增加某个固定值（通常是32）来转换成对应的小写字母。

## 参见
- Arduino String Reference: [https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/tolowercase/](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/tolowercase/)
- ASCII Code Table: [https://www.asciitable.com/](https://www.asciitable.com/)
