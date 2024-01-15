---
title:                "连接字符串"
html_title:           "Haskell: 连接字符串"
simple_title:         "连接字符串"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## 为什么

当我们需要将几个字符串合并成一个大字符串时，就需要使用concatenation（字符串拼接）功能。这在编写程序时非常有用，特别是在处理文本数据时。

## 怎样做

要使用Haskell中的字符串拼接功能，我们可以使用"++"操作符。下面是一个简单的例子：

```Haskell
myString = "Hello" ++ "World"
```

这将创建一个新的字符串，"HelloWorld"，并将其赋值给myString变量。我们也可以将多个字符串连接在一起：

```Haskell
combinedString = "I" ++ " " ++ "love" ++ " " ++ "Haskell!"
```

这将创建一个新的字符串，"I love Haskell!"，并将其赋值给combinedString变量。请注意，我们需要使用空格字符串来确保单词之间有空格。

我们也可以使用函数来拼接字符串。比如，函数`concat`可以将列表中的所有字符串连接在一起：

```Haskell
myList = ["I", "love", "Haskell!"]
combinedString = concat myList
```

在这个例子中，combinedString的值将是"I love Haskell!"。

## 深入了解

除了使用"++"操作符和`concat`函数之外，Haskell还提供了其他一些方法来拼接字符串。比如，我们可以使用`intercalate`函数来在字符串列表中插入一个分隔符：

```Haskell
myList = ["apple", "banana", "orange"]
combinedString = intercalate ", " myList
```

这将创建一个新的字符串，"apple, banana, orange"，并将其赋值给combinedString变量。

我们还可以使用`foldl`或`foldr`函数来在字符串列表中添加一些额外的逻辑。比如，我们可以使用`foldl`函数来计算字符串列表中所有字符串的长度，并将它们连接在一起：

```Haskell
myList = ["Hello", "World", "!"]
combinedString = foldl (\acc x -> acc ++ " " ++ show (length x)) "" myList
```

在这个例子中，combinedString的值将是"5 5 1"，因为Hello有5个字符，World有5个字符，而"!"只有1个字符。

## 参考资料

- [Haskell的字符串文档](https://haskell.org/tutorial/strings.html)
- [Haskell的字符串函数](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-List.html)
- [Haskell的fold函数](https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#v:foldl)

## 参见

- [Haskell的基础知识](https://www.hellohaskell.com/)
- [Haskell的字符串处理教程](https://www.tutorialspoint.com/haskell/haskell_strings.htm)