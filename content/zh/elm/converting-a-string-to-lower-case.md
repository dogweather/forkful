---
title:                "Elm: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# 为什么要把字符串转换为小写？

在编程中，经常会遇到需要将字符串转换为小写的情况。这在处理用户输入或数据比较时非常有用，因为它能帮助我们忽略大小写而获得更准确的结果。

## 如何进行转换？

在Elm中，我们可以通过内置函数`String.toLower`来将字符串转换为小写。下面是一个示例代码和输出：

```Elm
name = "ELM PROGRAMMING"
lowercase = String.toLower name
-- 输出: "elm programming"
```

## 深入了解

更深入地说，`String.toLower`函数会根据Unicode字符集来转换字符串。它会将所有大写字母转换为对应的小写字母，而对于其他非字母字符则不会做任何改变。

另外，有些情况下我们可能需要将字符串中的特定字符转换为小写，这时我们可以使用`String.map`函数结合`Char.toLower`来实现。下面是一个示例代码和输出：

```Elm
sentence = "I Love Elm!"
lowercaseSentence = String.map Char.toLower sentence
-- 输出："i love elm!"
```

# 参考链接

- Elm官方文档：https://guide.elm-lang.org/strings.html#modifiers
- Unicode字符集：https://unicode-table.com/en/#basic-latin
- String模块文档：https://package.elm-lang.org/packages/elm/core/latest/String