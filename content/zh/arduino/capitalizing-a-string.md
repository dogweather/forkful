---
title:    "Arduino: 编程中的大写字符串"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

#为什么：为什么要学习如何在Arduino中将字符串大写？
在编程中，有时候我们需要将字符串转换为大写，例如当我们需要比较字符串时。学习如何在Arduino中将字符串大写可以帮助我们更轻松地处理字符串数据。

##如何进行：
首先，我们需要使用Arduino的`String`类来声明一个字符串变量，例如`String text = "hello world";`。然后，我们可以使用`toUpperCase()`函数来将该字符串转换为大写，代码如下：
```Arduino
String text = "hello world";
text.toUpperCase(); //此时text的值为"HELLO WORLD"
```
我们也可以使用`+`运算符来将两个字符串合并，并将其转换为大写：
```Arduino
String text1 = "hello";
String text2 = "world";
String text3 = text1 + text2;
text3.toUpperCase(); //此时text3的值为"HELLO WORLD"
```
在Arduino中，我们还可以使用自定义函数来将某个字符串中的字母逐个转换为大写。代码示例如下：
```Arduino
void toUpperCase(String& str) {
  for (int i = 0; i < str.length(); i++) {
    str.setCharAt(i, toupper(str.charAt(i))); 
  }
}

String text = "hello world";
toUpperCase(text); //此时text的值为"HELLO WORLD"
```

##深入了解：
在C++中，字符串被存储在字符数组中，每个字符对应一个ASCII码。ASCII码中，小写字母从97（a）开始，大写字母从65（A）开始，它们的差值为32。因此，将小写字母转换为大写字母，只需要将其ASCII码减去32即可。在前面的例子中，`toUpperCase()`函数和自定义函数的作用原理实际上就是这样。

##参考资料：
- [Arduino官方文档 - String类](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [ASCII码表](https://www.asciitable.com/)