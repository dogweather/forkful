---
title:                "Arduino: 使用正则表达式"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 为什么要使用正则表达式

正则表达式是一种强大的工具，可以帮助程序员处理和匹配各种文本数据。它们可以快速而准确地搜索和替换文本，用来验证输入数据以及解析格式化的文本。因此，如果您在Arduino编程中遇到需要处理文本的情况，那么正则表达式绝对是您的好帮手。

## 如何使用正则表达式

在Arduino编程中，您可以使用Regexp库来实现正则表达式的功能。首先，您需要在Arduino IDE中安装Regexp库，并在您的程序中包含`#include <Regexp.h>`语句。

接着，在您的程序中创建一个Regexp对象，可以通过传入正则表达式的模式来初始化Regexp对象。例如，`Regexp pattern("hello (\\w+)");`将会创建一个用于匹配以“hello”开头并且后面跟着至少一个单词的文本的正则表达式对象。

接下来，您可以使用`match()`函数来检查您想要匹配的文本是否符合正则表达式的模式。如果成功匹配，则可以使用`group()`函数来提取匹配的文本。

让我们来看一个简单的例子，假设我们想要从输入的文本中提取出所有的数字。首先，我们需要定义一个正则表达式对象，`Regexp digits("\\d+");`，然后通过调用`match()`函数来检验输入的文本是否包含数字。如果匹配成功，我们可以使用`group()`函数来提取匹配的文本，例如`Serial.println(digits.group());`将会打印出匹配的数字文本。

```Arduino
#include <Regexp.h>

Regexp digits("\\d+"); // 创建正则表达式对象
String input = "There are 1234 apples in the basket."; // 定义输入文本
if(digits.match(input)){ // 检验输入的文本是否包含数字
  Serial.println("Match found!");// 若匹配成功
  Serial.println(digits.group()); // 打印出匹配的文本
} else {
  Serial.println("No match found."); // 若匹配失败
}
```

输出结果将会是：

```
Match found!
1234
```

## 深入了解正则表达式

正则表达式中有很多特殊的元字符和语法，可以帮助您创建更加复杂和精确的模式匹配。例如，使用括号可以创建一个子表达式，方便在匹配时提取特定的文本。使用量词可以指定匹配的次数，如`+`表示匹配一次或多次，而`*`表示匹配零次或多次。

此外，正则表达式还支持多个修饰符，如`i`表示大小写不敏感，`g`表示全局匹配。通过深入了解这些特殊元字符、语法和修饰符，您可以更加灵活和精准地处理文本数据。

详细的正则表达式语法和示例可以在[这里](https://github.com/apache/couchdb-documentation/blob/master/src/couchapp/ddocs/native/show.regexp.md)找到。

## 参考资料

- Regexp库：https://github.com/apache/couchdb-documentation/blob/master/src/couchapp/ddocs/native/show.regexp.md
- 了解正则表达式语法：https://github.com/apache/couchdb-documentation/blob/master/src/couchapp/ddocs/native/show.regexp.md
- 了解正则表达式修饰符：https://www.w3schools.com/jsref/jsref_obj_regexp.asp