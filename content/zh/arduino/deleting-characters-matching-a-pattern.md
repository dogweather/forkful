---
title:                "Arduino: 删除匹配模式的字符"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

为什么：为什么会有人想要删除匹配特定模式的字符，这是本文的主要内容。

## 为什么

在Arduino编程中，有时候我们需要对字符串进行处理，比如去除其中一些特定的字符。而删除匹配特定模式的字符则可以帮助我们更有效地处理字符串，减少错误和不必要的字符。

## 如何做

让我们来看一个简单的例子，在Arduino中使用这种方法来删除字符串中的所有空格。首先，我们需要用一个字符串变量来存储我们想要处理的字符串。

```
Arduino string str = "Hello World";
```

接下来，我们使用一个for循环来遍历字符串中的每个字符，同时使用`if`语句来判断当前字符是否为空格。如果是空格，则使用`erase`函数来删除该空格，最后将处理后的字符串打印出来。

```
for (int i = 0; i < str.length(); i++){
  if (str[i] == ' '){
    str.erase(i, 1);
  }
}
Serial.println(str);
```

以上代码的输出将为`HelloWorld`，实现了删除空格的目的。

## 深入讨论

除了上面的简单示例，我们还可以扩展这种方法来处理更复杂的字符串操作。我们可以使用正则表达式来匹配特定模式的字符，并使用相应的函数来删除这些字符。此外，我们还可以将这种方法与其他字符串处理方法结合起来，实现更多功能。

## 请看

- [Arduino String类](https://www.arduino.cc/reference/zh/language/variables/data-types/stringobject/)
- [C++中的正则表达式](https://www.tutorialspoint.com/cpp_standard_library/regex.htm)
- [Markdown语法指南](https://lise-henry.github.io/markdown-toc/)