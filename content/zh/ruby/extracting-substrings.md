---
title:                "Ruby: 提取子字符串"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

# 为什么

有时候，我们需要从一个字符串中提取特定的一部分内容。这可能是因为我们只需要处理字符串的一部分，或者我们需要将字符串的一部分作为参数传递给其他函数。无论是什么原因，提取子字符串都是非常常见的编程任务，而在Ruby中，这项任务变得非常简单。

# 如何做

Ruby提供了几种方法来提取子字符串。让我们来看看最常用的三种方法：`slice`、`substring`和`scan`。假设我们有一个包含妈妈们日记的字符串，我们想提取出每一段的文字。

```Ruby
diary = "今天，妈妈和我一起去公园玩耍。我非常开心。妈妈还给我买了冰淇淋。"
```

## Slice

使用`slice`，我们可以指定一个起始索引和一个结束索引来提取子字符串。

```Ruby
diary.slice(3,7)
```

输出为`"妈妈和我一起去公园玩耍"`，因为我们从索引3（`妈`）开始提取，到索引9（`耍`）结束。

## Substring

`substring`和`slice`的用法非常类似，但是可以指定一个起始索引和一个长度来提取子字符串。

```Ruby
diary.substring(12,4)
```

输出为`"我非常开心"`，因为我们从索引12（`非`）开始提取，提取4个字符。

## Scan

最后，`scan`方法允许我们使用正则表达式来提取子字符串。

```Ruby
diary.scan(/妈妈(.*?)。/i)
```

这个正则表达式指定了我们想要提取的子字符串的模式，输出为`["妈妈和我一起去公园玩耍", "还给我买了冰淇淋"]`，因为它匹配了两次。

# 深入分析

通过使用这三种不同的方法，我们可以根据我们的需求来提取子字符串。但是需要注意的是，`slice`和`substring`方法都是基于字符串的索引，而`scan`方法则是基于正则表达式。所以，在使用这些方法时，一定要考虑清楚你需要的是基于索引还是模式来提取。

# 参考链接

- Ruby文档：https://ruby-doc.org/core-2.7.1/String.html#method-i-slice
- Ruby正则表达式指南：https://www.tutorialspoint.com/ruby/ruby_regular_expressions.htm
- Ruby教程：https://ruby-china.org/wiki/ruby/lesson/11