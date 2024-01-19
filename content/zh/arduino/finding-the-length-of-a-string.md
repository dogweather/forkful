---
title:                "查找字符串的长度"
html_title:           "Javascript: 查找字符串的长度"
simple_title:         "查找字符串的长度"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 什么是原因？ "What & Why?"

在Arduino编程中，我们经常需要找出字符串的长度。之所以这样做，是因为很多数据处理和运算都需要知道这个信息。

## 如何操作 "How to:"

使用Arduino的`strlen`函数可以直接获取字符串长度。看下面的例子:

```Arduino
char str[] = "Arduino";
int len;

void setup() {
  Serial.begin(9600);
  len = strlen(str);
  Serial.println(len);
}

void loop() {
  // the rest of your code
}
```

这样，串行监视器将会显示字符串“Arduino”的长度，也就是7。

## 深入了解 "Deep Dive"

1. 历史背景: `strlen`函数是最古老的字符串操作函数之一，源于C语言。

2. 可替代项: Arduino里还有其他几种测算字符串长度的函数，比如`String.length()`，它是首选的方法之一，特别是在使用String类时。

3. 实现细节: `strlen`函数其实就是通过一个循环，一个个计算字符，直到遇到字符串结束标志'\0'为止。这就是它如何知道字符串长度的。

## 参考资料 "See Also"

- Arduino函数库: [https://www.arduino.cc/reference/en/](https://www.arduino.cc/reference/en/)
- C语言字符串处理: [https://www.geeksforgeeks.org/c-string-library-strlen/](https://www.geeksforgeeks.org/c-string-library-strlen/)
- Arduino上的字符串: [https://startingelectronics.org/software/arduino/learn-to-program-course/10-strings/](https://startingelectronics.org/software/arduino/learn-to-program-course/10-strings/)