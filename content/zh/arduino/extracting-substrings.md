---
title:    "Arduino: 提取子串"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

# 为什么：从字符串中提取子字符串的重要性

如果你正在使用Arduino进行编程，你可能会遇到需要从一个字符串中提取特定的一部分来使用的情况。这种操作被称为提取子字符串，它可以帮助你更有效地使用字符串数据。下面我们就来学习如何在Arduino中提取子字符串。

## 如何做到

通过使用substring函数，我们可以轻松地从一个字符串中提取出我们需要的部分。请看以下示例代码：

```Arduino
// 声明一个字符串变量
String sentence = “Hello World”;

// 使用substring函数提取子字符串
String hello = sentence.substring(0, 5); // 从索引0开始，提取5个字符
String world = sentence.substring(6, sentence.length()); // 从索引6开始，提取到最后一个字符

// 打印结果
Serial.println(hello); // 输出：Hello
Serial.println(world); // 输出：World
```

在上面的例子中，我们首先声明了一个名为sentence的字符串变量，它包含了“Hello World”的值。然后，我们使用substring函数来提取子字符串，第一个参数指定了开始提取的索引，第二个参数指定了提取的字符数量。最后，我们使用串口监视器（Serial Monitor）来打印结果，可以看到我们成功地从sentence中提取出了想要的子字符串。

## 深入学习

除了指定开始索引和提取的字符数量，substring函数还有一个可选的第三个参数，它可以用来指定你想要提取的字符类型。默认情况下，它提取的是字符串型。但是如果你在第三个参数中填入int类型的值，它会将提取的子字符串转换为整型。这个功能非常有用，特别是当你需要从字符串中提取数值类型的数据时。

另外，如果你需要提取的字符数量已知，你也可以使用charAt函数来提取单个字符，如下所示：

```Arduino
// 使用charAt函数提取单个字符
char firstChar = sentence.charAt(0); // 提取第一个字符
char lastChar = sentence.charAt(sentence.length() - 1); // 提取最后一个字符

// 打印结果
Serial.println(firstChar); // 输出：H
Serial.println(lastChar); // 输出：d
```

通过以上的例子，我们可以看到charAt函数提取的是char类型的值，它可以帮助我们更方便地处理单个字符。

# 请使用

现在你已经学会了如何在Arduino中提取子字符串，你可以将它应用到你的项目中。这项技能对于处理字符串数据非常有用，它可以帮助你更有效地操作和管理字符串。如果你想要进一步了解substring函数和charAt函数的更多用法，可以参考下面的链接。

# 查看也有用

- [Arduino substring函数文档](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/)
- [Arduino charAt函数文档](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/charAt/)