---
date: 2024-01-20 17:38:10.910041-07:00
description: null
isCJKLanguage: true
lastmod: '2024-04-05T21:53:48.343375-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u5C06\u5B57\u7B26\u4E32\u8F6C\u6362\u4E3A\u5C0F\u5199"
weight: 4
---

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
