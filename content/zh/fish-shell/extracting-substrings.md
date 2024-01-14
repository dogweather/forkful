---
title:                "Fish Shell: 提取子字符串"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## 为什么

有时候，在处理文本数据时，我们需要从一长串字符串中提取一部分内容。例如，从一条URL中提取网址域名。在这种情况下，提取子串就是非常有用的，它可以帮助我们快速地获取我们需要的信息。

## 如何

Fish Shell提供了内置的字符串提取函数，让我们可以轻松地提取指定位置的子串。下面是一个简单的例子，假设我们有一个包含学生姓名和成绩的字符串，每条记录以“|”分隔：

```
Fish Shell> set str "张三|89"
Fish Shell> echo $str[3..-1]
89
```

在上面的例子中，我们使用`str[3..-1]`提取了`str`中第3个字符（从0开始计数）到最后一个字符之间的部分，即89。我们也可以结合其他命令对提取的子串进行进一步的处理：

```
Fish Shell> echo $str[3..-1] | string sub -r '[0-9]+$'        
89
```

在上面的例子中，我们使用了Fish Shell的`string sub`命令来删除`str[3..-1]`提取的子串中的所有非数字字符，最终得到了我们需要的成绩。

## 深入探讨

除了上面为大家演示的简单例子，Fish Shell还提供了更多更强大的字符串提取函数，如`string match`和`string replace`。它们可以帮助我们更灵活地处理字符串，实现更复杂的提取和替换操作。同时，Fish Shell也支持使用正则表达式来进行字符串提取，极大地增强了提取子串的能力。

## 参考资料

- [Fish Shell文档-字符串提取函数](https://fishshell.com/docs/current/cmds/string.html)
- [Fish Shell文档-正则表达式](https://fishshell.com/docs/current/index.html#regex)
- [Fish Shell使用教程](https://fishshell.com/docs/current/index.html)