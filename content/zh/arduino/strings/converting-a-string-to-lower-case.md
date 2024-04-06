---
date: 2024-01-20 17:38:10.910041-07:00
description: ''
isCJKLanguage: true
lastmod: '2024-04-05T22:38:47.203564-06:00'
model: gpt-4-1106-preview
summary: "\u5982\u4F55\uFF1A \u5B57\u7B26\u4E32\u8F6C\u6362\u6210\u5C0F\u5199\u8FD9\
  \u4E2A\u64CD\u4F5C\u5B58\u5728\u5DF2\u4E45\uFF0C\u56E0\u4E3A\u5B83\u662F\u6587\u672C\
  \u5904\u7406\u4E2D\u7684\u57FA\u672C\u529F\u80FD\u3002\u9664\u4E86`String`\u7C7B\
  \u7684`toLowerCase()`\u65B9\u6CD5\u5916\uFF0C\u8FD8\u53EF\u4EE5\u901A\u8FC7\u904D\
  \u5386\u5B57\u7B26\u4E32\u4E2D\u7684\u6BCF\u4E2A\u5B57\u7B26\uFF0C\u5229\u7528ASCII\u7801\
  \u8868\u6765\u624B\u52A8\u8F6C\u6362\u3002\u4E0D\u8FC7\uFF0CArduino\u63D0\u4F9B\u7684\
  \u65B9\u6CD5\u66F4\u6613\u7528\uFF0C\u6548\u7387\u4E5F\u66F4\u9AD8\uFF0C\u5B83\u9690\
  \u85CF\u4E86\u5B9E\u73B0\u7EC6\u8282\u3002\u5185\u90E8\u5B9E\u73B0\u673A\u5236\u901A\
  \u5E38\u6D89\u53CA\u68C0\u67E5\u5B57\u7B26\u662F\u5426\u5728\u5927\u5199\u5B57\u6BCD\
  \u7684ASCII\u8303\u56F4\u5185\uFF0865-90\uFF09\uFF0C\u5982\u679C\u662F\uFF0C\u5C31\
  \u901A\u8FC7\u589E\u52A0\u67D0\u4E2A\u56FA\u5B9A\u503C\uFF08\u901A\u5E38\u662F32\uFF09\
  \u6765\u8F6C\u6362\u6210\u5BF9\u5E94\u7684\u5C0F\u5199\u5B57\u6BCD\u3002"
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
