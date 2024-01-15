---
title:                "删除匹配模式的字符"
html_title:           "Haskell: 删除匹配模式的字符"
simple_title:         "删除匹配模式的字符"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

为什么：在编程的过程中，我们经常会遇到需要删除特定模式字符的情况，这个技巧能帮助我们更高效地处理文本数据。

怎么做：我们可以使用Haskell中的`filter`函数来删除符合我们要求的字符。比如我们想要删除字符串中所有的小写字母,我们可以使用下面的代码：

```Haskell
filter (\x -> x `elem` ['A'..'Z']) "HelloWorld"
```

输出结果为`HW`.

深入探讨：我们可以进一步使用`map`函数来一次性删除多个字符。比如我们想要删除字符串中的所有数字和标点符号，我们可以使用下面的代码：

```Haskell
filter (\x -> not (x `elem` ['0'..'9'])) (map toLower "Hello, 123World!")
```

输出结果为`helloworld`。

另外，我们也可以使用`delete`函数来删除指定字符，该函数接受两个参数，第一个参数是要删除的字符，第二个参数是要删除的字符串。比如我们想要删除字符串中的所有感叹号，可以使用下面的代码：

```Haskell
delete '!' "Hello, World!!!"
```

输出结果为`Hello, World`.

值得注意的是，`delete`函数只能删除一个字符，如果我们想要一次性删除多个字符，可以使用`foldl`函数来实现。具体实现可以参考`delete`函数的源码。

另外，如果我们想要删除字符串中的所有空格，可以使用`concatMap`函数来实现，具体实现可以参考`dropSpace`函数的源码。

查看更多例子可以参考[Haskell的文档](https://haskell.org/documentation)。

## 参考链接

- [Haskell文档](https://haskell.org/documentation)
- [Haskell语言序列教程](https://www.haskell.org/tutorial/index.html)
- [Haskell语言官网](https://www.haskell.org/)