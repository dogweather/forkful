---
title:    "Elixir: 提取子字符串"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## 为什么

很多时候，在编程中，我们需要从一个字符串中提取一部分内容。这种情况经常发生，比如我们需要从用户输入的字符串中提取一个特定的单词，或者我们想要从一个长文本中提取出一句话。在Elixir中，我们可以使用一些简单的方法来提取子字符串。

## 如何实现

首先，我们需要一个原始字符串来提取子字符串。假设我们有一个包含名字和姓氏的字符串，比如"张三李四". 我们可以使用Elixir的String模块中的`slice`方法来提取姓氏：

```Elixir
str = "张三李四"
String.slice(str, 2..3)
```

这将返回一个新字符串 "李四" 。我们也可以使用`match`方法来从字符串中提取出特定的部分，比如提取出 "张三"：

```Elixir
"张三李四" |> String.match(~r/张三/) |> hd
```

上面的代码使用了正则表达式来匹配字符串中的 "张三"，然后通过管道操作符将匹配到的结果传递给`hd`方法来获取第一个匹配项 "张三"。

另一种常用的方法是使用字符串的索引来提取子字符串。比如我们可以使用`String.codepoints`来将字符串拆分为单个字符的列表，然后通过索引来提取出需要的部分。比如提取出第一个字 "张"：

```Elixir
"张三李四" |> String.codepoints |> Enum.at(0)
```

这将返回 "张"。

## 深入了解

在Elixir中，字符串是不可变的，因此所有提取子字符串的方法都会返回一个新的字符串。同时，Elixir中的字符串也是二进制数据，因此提取出的子字符串也仍然保留着二进制的属性，可以通过`binary_id`方法来验证。

另外，除了上面提到的方法，我们还可以使用`String.split`来将字符串根据某个分隔符拆分为列表，然后再提取出需要的部分。比如我们可以使用空格作为分隔符，提取出名字 "张三"：

```Elixir
"张三 李四" |> String.split(" ") |> Enum.at(0)
```

## 参考链接

- [Elixir官方文档-String模块](https://hexdocs.pm/elixir/String.html)
- [正则表达式教程](https://www.runoob.com/regexp/regexp-syntax.html)
- [Elixir字符串方法总结](https://www.jianshu.com/p/d59efb21a1b5)

---
## 参见

- [正则表达式入门指南](https://www.jianshu.com/p/35c223795edb)
- [Elixir字符串操作指南](https://elixirschool.com/zh-cn/lessons/basics/binary-and-strings/)