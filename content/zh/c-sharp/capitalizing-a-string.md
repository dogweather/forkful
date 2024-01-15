---
title:                "字符串大写处理"
html_title:           "C#: 字符串大写处理"
simple_title:         "字符串大写处理"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 为什么

有时候，我们需要在程序中将字符串的首字母大写。这可能是因为我们想要规范化用户输入的数据，或者美化输出结果。无论是什么原因，C＃提供了一种简单的方法来实现这一目的。

## 如何

在C＃中，我们可以使用```string.ToTitleCase()```方法来将字符串的首字母大写。下面是一个简单的例子：

```C#
string str = "hello world";
Console.WriteLine(str.ToTitleCase());  //输出结果为 "Hello World"
```

我们可以看到，在这个例子中，我们首先定义了一个字符串变量 ```str```，然后使用 ```ToTitleCase()```方法将其转换成首字母大写的形式，并将结果打印出来。这个方法会自动识别单词的边界，并将每个单词的首字母大写，其他字母小写。

另外，如果我们想要将字符串的所有字母都转换成大写，可以使用```string.ToUpper()```方法。同样，如果想要转换成全部小写，可以使用```string.ToLower()```方法。

## 深入探讨

在C＃中，字符串是不可变的对象。这意味着一旦我们创建了一个字符串，就无法对其进行修改。因此，当我们调用 ```ToTitleCase()```方法时，实际上是创建了一个新的字符串对象，而不是在原来的字符串上进行修改。

另外值得注意的是，```ToTitleCase()```方法也会考虑当前的语言文化设置，因此在不同的语言环境下，可能会有不同的结果。

## 查看也许

- [Microsoft官方文档：String.ToTitleCase 方法](https://docs.microsoft.com/zh-cn/dotnet/api/system.string.totitlecase)
- [C# 字符串转换为大写和小写](https://www.w3cschool.cn/csharp/csharp-string-toupper.html)