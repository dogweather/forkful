---
title:                "Arduino: 使用正则表达式"
simple_title:         "使用正则表达式"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

# 为什么要使用正则表达式？

在编写Arduino程序时，您可能会遇到需要对输入数据进行特定格式的匹配或提取字符串的情况。这时，正则表达式就能帮上大忙了。它是一种强大的工具，可以根据指定的规则在字符串中进行匹配和替换，使得数据处理更加方便快捷。

# 如何使用正则表达式？

要在Arduino中使用正则表达式，我们需要用到软件库Regex。首先，在库管理器中搜索Regex，然后安装它。接着，我们需要设置一个Regex对象，指定我们想要匹配的模式。下面是一个简单的例子，它将匹配一个由3个数字组成的字符串，并将其转换为整数类型。

```Arduino
#include <Regex.h>

void setup() {
  // 设置Regex对象，指定我们想要匹配的模式
  Regex regex("[0-9]{3}");
  
  // 创建一个字符串用于匹配
  String input = "123";
  
  // 使用match()方法进行匹配，并将结果转换为整数类型
  int result = regex.match(input).toInt();
  
  // 打印结果
  Serial.println(result); // 输出：123
}

void loop() {
  // 程序循环
}
```

这只是一个简单的例子，您可以使用正则表达式来实现更复杂的数据处理任务，如验证电子邮件地址或提取URL链接等。

# 深入了解正则表达式

正则表达式的语法相对复杂，需要一定的学习和练习才能熟练掌握。它包含各种特殊字符和模式，可以根据需要进行灵活的组合。您可以通过查阅正则表达式的参考资料或参加相关的课程来深入了解它的用法和常见的编写错误。

# 参考资料

- Regex软件库：https://www.arduino.cc/reference/en/libraries/regex/
- Arduino正则表达式教程：https://www.arduino.cc/reference/en/language/functions/communication/regex/
- 正则表达式基础知识：https://www.w3schools.com/js/js_regexp.asp

# 参见

- Arduino官方参考文档：https://www.arduino.cc/reference/en/