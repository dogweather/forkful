---
title:    "Arduino: 将字符串转换为小写"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 为什么

在编写Arduino程序时，有时会需要将字符串转换为小写格式，以满足特定的需求。例如，当需要比较两个字符串时，为了避免大小写造成的差异，就需要将它们都转换成小写。

## 如何做到

要将字符串转换为小写格式，可以使用Arduino的`toLowerCase()`函数。该函数接受一个字符串参数，并返回一个新的字符串，其中所有的字符均转换为小写。

```
Arduino String str = "HELLO WORLD";
Arduino String lowerStr = str.toLowerCase();
```

下面是一个完整的示例程序，在串口监视器中打印出转换前后的字符串：

```
void setup() {
  Serial.begin(9600);
}

void loop() {
  String str = "Hello World";
  Serial.println(str);

  String lowerStr = str.toLowerCase();
  Serial.println(lowerStr);

  delay(1000);
}
```

输出结果如下所示：

```
Hello World
hello world
```

## 深入了解

在Arduino中，字符串是使用C语言的`char`数组来表示的。而C语言本身并没有直接的函数来将字符串转换为小写格式，所以Arduino的`toLowerCase()`函数实际上是实现了以下的算法：

```
// 遍历字符串的每个字符
for (int i = 0; i < str.length(); i++) {
  // 获取当前字符的ASCII码
  int ascii = str.charAt(i);

  // 如果是大写字母，则加上32转换为小写
  if (ascii >= 65 && ascii <= 90) {
    ascii += 32;
  }

  // 将转换后的ASCII码重新赋值给原来的字符
  str.setCharAt(i, ascii);
}
```

因此，如果需要在Arduino以外的环境中使用类似的功能，可以参考这段代码来实现字符串转换为小写的功能。

## 参考链接

- [Arduino官方文档 - toLowerCase()](https://www.arduino.cc/reference/en/language/variables/data-types/stringfunctions/tolowercase/)
- [C语言字符串基础知识](https://www.cprogramming.com/tutorial/c/lesson9.html)

## 参见

- [字符串比较指南](https://example.com/string-comparison)
- [如何在Arduino中处理字符串](https://example.com/arduino-string-processing)