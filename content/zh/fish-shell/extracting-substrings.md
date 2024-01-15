---
title:                "提取子字符串"
html_title:           "Fish Shell: 提取子字符串"
simple_title:         "提取子字符串"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## 为什么

提取子字符串是很常见的编程任务，它可以帮助我们更方便地处理文本数据。如果你想精确地匹配或处理特定的文本，提取子字符串是非常有用的。

## 如何进行

```fish
set string "This is a sample string"
echo $string[1,4]
```
这段代码将在Fish Shell中输出 "This"，它使用了字符串变量以及子字符串的语法来提取字符串"string"中的前四个字符。

而如果我们想从一个字符串的特定位置开始提取子字符串，可以使用如下代码：

```fish
set string "This is a sample string"
echo $string[9:]
```
这将输出 "sample string"，因为我们指定了从第九个字符开始提取直到末尾。

除此之外，我们还可以使用如下代码来提取一个固定长度的子字符串：

```fish
set string "This is a sample string"
echo $string[3:7]
```
这将输出 "is is"，因为我们指定了从第三个字符开始提取，长度为七个字符。

## 深入了解

除了基本的用法外，还有一些重要的细节需要知道。首先是Fish Shell中使用的子字符串语法，它使用了类似于数组的形式来表示字符串的每个字符。通过指定起始和结束位置，我们可以提取出想要的子字符串。另外，如果我们想要提取最后一个字符，可以使用负数来表示，例如-1表示最后一个字符。

另一个需要注意的重要细节是子字符串的索引起始值为1而不是0，这与其他编程语言有些不同。因此，要注意单独提取第一个字符时，写成 `$string[1]` 而不是 `$string[0]`。

## 参考链接

- <https://fishshell.com/docs/current/tutorial.html#syntax>
- <https://fishshell.com/docs/current/commands.html#string-slicing>
- <https://fishshell.com/docs/current/index.html#performance>