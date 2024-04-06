---
date: 2024-01-20 17:46:47.494508-07:00
description: ''
isCJKLanguage: true
lastmod: '2024-04-05T21:59:59.591659-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u505A\uFF1F) \u83B7\u53D6\u5B57\u7B26\u4E32\u957F\u5EA6\u8FD9\
  \u529F\u80FD\uFF0C\u4ECEC\u8BED\u8A00\u7684`strlen()`\u51FD\u6570\u7EE7\u627F\u800C\
  \u6765\uFF0C\u66FE\u662F\u5904\u7406C\u5B57\u7B26\u4E32\u7684\u901A\u7528\u65B9\u5F0F\
  \u3002\u5728Arduino\u4E2D\uFF0C`String`\u5BF9\u8C61\u7684`.length()`\u65B9\u6CD5\
  \u63D0\u4F9B\u4E86\u8FD9\u4E00\u529F\u80FD\uFF0C\u5B83\u66F4\u4E3A\u5B89\u5168\u4E14\
  \u5BB9\u6613\u4F7F\u7528\u3002\u867D\u7136`strlen()`\u662F\u6D4B\u91CF\u5B57\u7B26\
  \u6570\u7EC4\u957F\u5EA6\u7684\u5FEB\u901F\u65B9\u6CD5\uFF0C\u4F46\u8FD9\u6D89\u53CA\
  \u6307\u9488\u64CD\u4F5C\uFF0C\u5BF9\u521D\u5B66\u8005\u6765\u8BF4\u53EF\u80FD\u8F83\
  \u4E3A\u590D\u6742\uFF0C\u800C\u4E14\u5982\u679C\u6CA1\u6709\u6B63\u786E\u5730\u4F7F\
  \u7528\u7ED3\u675F\u5B57\u7B26'\\0'\uFF0C\u5219\u53EF\u80FD\u5F15\u8D77\u9519\u8BEF\
  \u3002Arduino\u7684`String`\u7C7B\u5C01\u88C5\u4E86\u8FD9\u4E9B\u7EC6\u8282\uFF0C\
  \u8BA9\u5B57\u7B26\u4E32\u64CD\u4F5C\u53D8\u5F97\u66F4\u52A0\u7B80\u5355\u548C\u5B89\
  \u5168\u3002\u4F7F\u7528`.length()`\u4E0D\u9700\u8981\u62C5\u5FC3\u5B57\u7B26\u4E32\
  \u7ED3\u5C3E\u6216\u5185\u5B58\u6EA2\u51FA\u95EE\u9898\u3002"
title: "\u83B7\u53D6\u5B57\u7B26\u4E32\u7684\u957F\u5EA6"
weight: 7
---

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
