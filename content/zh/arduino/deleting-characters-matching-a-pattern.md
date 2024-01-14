---
title:                "Arduino: 删除匹配模式的字符"
simple_title:         "删除匹配模式的字符"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# 为什么

## Why

最近，我学习了如何使用Arduino编程，发现一个很有趣的功能：删除与特定模式匹配的字符。如果你对Arduino编程有所了解，也许你会好奇为什么要进行这样的操作。实际上，删除字符匹配功能在很多实际应用中都很有用，比如去除无效的数据或者改正错误的输入。这个功能不仅仅在Arduino编程中有用，也可以在其他领域得到应用，因此学习如何删除字符匹配是非常有用的！

# 怎么做

## How To

首先，我们需要准备一个Arduino主板和电脑。然后，创建一个新的Arduino项目并打开Serial Monitor。

```Arduino
void setup () {
  Serial.begin(9600);
}
void loop () {
  // 在此处编写代码
}
```

现在，我们需要使用一个字符串（可以是从传感器或用户输入获取的）作为示例数据。假设我们的字符串是"Hello World!"，现在我们想要删除其中所有的空格。我们可以使用Arduino中的replace()函数来实现这个目的。

```Arduino
  String data = "Hello World!";
  data.replace(" ", ""); // 这里是我们要删除的字符
  Serial.println(data); // 输出结果为 "HelloWorld!"
```

如果我们想要删除多个不同的字符，可以使用一个循环来遍历每个字符，并使用replace()函数来删除它们。

```Arduino
  String data = "Hello World!";
  char toRemove[] = {' ', '!'};
  for (int i = 0; i < 2; i++) {
    data.replace(String(toRemove[i]), "");
  }
  Serial.println(data); // 输出结果为 "HelloWorld"
```

# 深入探究

## Deep Dive

在Arduino的函数库中，replace()函数的定义如下：

```Arduino
replace( char toReplace, char with )
```

它有两个参数，第一个是要被替换的字符，第二个是要替换成的字符。我们可以使用这个函数来实现字符串中任何符合特定模式的字符的删除。值得注意的是，replace()函数只能替换字符串中的第一个匹配项，因此如果我们想要全部替换匹配的字符，需要使用一个循环来多次调用replace()函数。

另外，我们也可以使用其他方法来删除特定模式的字符，比如使用字符串的substring()函数来获取子串并进行拼接，或者使用正则表达式来匹配和替换字符。在选择方法时，需要根据实际场景和需求来决定应该使用什么方式。

# 更多学习资源

## See Also

1. [Arduino官方网站](https://www.arduino.cc/)
2. [《Arduino编程入门》 - 书籍推荐](https://book.douban.com/subject/26716354/)
3. [Arduino汉语入门教程 - 视频教程推荐](https://www.bilibili.com/video/BV1iw41127NZ?from=search&seid=5211939329808416431)