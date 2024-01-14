---
title:    "Fish Shell: 提取子字符串."
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## 为什么要提取子字符串？

在编写程序的过程中，有时候我们需要从一个字符串中提取出特定的部分，比如姓名中的姓氏或者日期中的年份。使用Fish Shell中提供的子字符串提取方法，可以轻松地实现这一需求。

## 如何提取子字符串？

Fish Shell中有两种方法可以提取子字符串：`string sub`和`string match`。`string sub`可以从一个字符串中提取出指定的字符，而`string match`则可以从字符串中匹配出符合某个模式的子字符串。下面是两个示例代码及其输出。

```
Fish Shell Code:
set str "Hello World!"
echo $str[1,5]

Output:
Hello
```

```
Fish Shell Code:
set str "April 23, 2021"
string match -r, $str
echo $string_match

Output:
23
```

## 深入了解子字符串提取

除了基本的提取方法外，Fish Shell还提供了更多的选项来满足不同情况下的需求。比如，使用`string sub`的`-r`选项可以实现反向提取，即从字符串的末尾开始提取。使用`string match`的`-a`选项可以提取出所有匹配的子字符串，而不仅仅是第一个。此外，Fish Shell还支持正则表达式，让我们能够更灵活地提取出符合特定模式的子字符串。

## 参考链接

- [Fish Shell官方文档](https://fishshell.com/docs/current/index.html)
- [Fish Shell提供的字符串操作方法](https://fishshell.com/docs/current/cmds/string.html)
- [正则表达式入门教程](https://www.runoob.com/regexp/regexp-tutorial.html)

## 相关链接

- [在Fish Shell中处理字符串的实用技巧](https://www.example.com)
- [如何在Fish Shell中使用变量和循环](https://www.example.com)