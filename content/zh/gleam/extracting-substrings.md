---
title:    "Gleam: 提取子字符串。"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

# 为什么要提取子串？

提取子串是一种有用的编程技巧，它可以从字符串中获取重要的信息并进行处理。例如，当处理大量的文本数据时，提取子串可以帮助我们快速地找到需要的数据，节省时间和精力。同时，提取子串也是解决字符串相关问题的一种有效方法。

# 如何提取子串？

要提取一个子串，我们可以使用Gleam提供的内置函数`substring`。这个函数接收三个参数：原始字符串、子串的起始索引和结束索引。在下面的示例中，我们将从字符串`"Hello World!"`中提取子串`"World"`。

```Gleam
let string = "Hello World!"
let substring = substring(string, 6, 10)
```

运行以上代码，输出的结果将是`"World"`。值得注意的是，子串的结束索引是不包含在提取范围内的，所以我们需要将结束索引设置为想要提取子串末尾的索引加一。

# 深入了解提取子串

除了`substring`函数，Gleam还提供了其他一些有用的函数来处理子串。例如，`left`和`right`函数可以从字符串的左侧和右侧提取指定长度的子串，而`match`函数可以根据正则表达式从字符串中提取匹配的子串。同时，我们也可以使用模式匹配来提取特定格式的子串，这在处理复杂的字符串时非常有用。

# 同时查看

- [Gleam文档：字符串操作](https://gleam.run/book/tour/string.txt)
- [正则表达式在线测试工具](https://regex101.com/)