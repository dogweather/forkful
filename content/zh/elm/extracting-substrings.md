---
title:                "提取子字符串"
html_title:           "Elm: 提取子字符串"
simple_title:         "提取子字符串"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## 为什么

你可能想提取子字符串来处理一些比较复杂的文本数据，例如在搜索功能中查找匹配的单词或者为了提取更精确的信息。使用Elm语言提取子字符串可以帮助你高效地处理这些文本数据。

## 怎么做

```Elm
-- 从字符串中提取子字符串
String.slice start end str
```

这里的start和end是整数值，表示你想提取的子字符串的起始和结束位置，str是你想要提取子字符串的原始字符串。如果你想从第一个字符开始提取，你可以将start的值设置为0，end的值可以根据你想提取的子字符串长度来调整。

```Elm
-- 示例输出
String.slice 3 7 "Hello World!"  == "lo W"
```

提取子字符串的一个常用方法是使用索引和字符串长度来计算start和end的值，以便确保提取的字串的范围恰好是你需要的。

## 深入探讨

除了使用String.slice函数来提取子字符串外，Elm还提供了一些其他的函数来处理字符串数据。比如，可以使用String.left或者String.right函数来从左边或右边提取指定长度的子字符串。

```Elm
-- 从左边提取10个字符
String.left 10 "This is a string!" == "This is a "
```

另外，使用String.contains函数可以检查一个字符串是否包含另外一个字符串，这在搜索功能中非常有用。

```Elm
-- 检查字符串是否包含特定的单词
String.contains "apple" "I love eating apples." == True
```

## 参考链接

- Elm官方文档：https://elm-lang.org/docs
- Elm中国社区：https://elmchina.org/
- Elm字符串模块文档：https://package.elm-lang.org/packages/elm/core/latest/String