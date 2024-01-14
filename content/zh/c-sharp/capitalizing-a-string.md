---
title:                "C#: 大写字符串"
simple_title:         "大写字符串"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# 为什么要将字符串首字母大写

在编程领域中，有时需要将字符串中的首字母大写。这可能是因为显示、排序或其他目的而需要。虽然C#提供了许多内置的方法来处理字符串，但是我们仍然需要知道如何将字符串的首字母大写。在本文中，我们将学习如何使用C#来实现这一目的。

## 如何实现

为了将字符串的首字母大写，我们可以使用C#中的`ToUpper()`方法。例如，让我们创建一个字符串变量`name`并赋值为`"john"`。然后，我们可以使用`ToUpper()`方法来将首字母大写，并将结果赋值给一个新的变量`capitalizedName`。代码如下所示：

```C#
string name = "john";
string capitalizedName = name.ToUpper();
```
运行上述代码将得到结果`JOHN`。

除了使用`ToUpper()`方法，我们也可以使用`char.ToUpper()`方法将字符转换为大写。因此，我们也可以使用以下代码来将字符串的首字母大写：

```C#
//假设字符串只有一个单词
string name = "john";
string capitalizedName = char.ToUpper(name[0]) + name.Substring(1);
```

## 深入了解

虽然使用`ToUpper()`或`char.ToUpper()`方法可以很容易地将字符串的首字母大写，但是我们也可以自己实现这一功能。一个简单的方法是将字符串转换为字符数组，然后将第一个字符转换为大写，最后再将字符数组转换回字符串。代码如下所示：

```C#
string name = "john";
char[] chars = name.ToCharArray();
chars[0] = char.ToUpper(chars[0]);
string capitalizedName = new string(chars);
```

除此之外，我们也可以使用正则表达式来将字符串的首字母大写。通过使用正则表达式，我们可以对字符串进行更复杂的操作，例如将每个单词的首字母大写等。代码如下所示：

```C#
string name = "john smith";
string capitalizedName = Regex.Replace(name, @"\b[a-z]", (match) => match.Value.ToUpper());
```

## 参考资料

- [C# 文本处理指南](https://docs.microsoft.com/en-us/dotnet/api/system.string?view=netcore-3.1)
- [如何在C#中转换字符串为大写首字母](https://www.c-sharpcorner.com/UploadFile/8911c4/how-to-convert-first-letter-of-string-to-uppercase-in-C-Sharp/)
- [使用正则表达式转换字符串为大写首字母](https://www.c-sharpcorner.com/blogs/capitalize-first-letter-of-each-word-in-string-using-regular-expressions1)

# 参考资料

- [Markdowm 语法指南](https://www.markdownguide.org/basic-syntax/)
- [C# 字符串处理方法](https://docs.microsoft.com/zh-cn/dotnet/api/system.string?view=netcore-3.1)
- [如何在C#中将字符串的首字母大写](https://www.c-sharpcorner.com/UploadFile/8911c4/How-to-convert-first-letter-of-string-to-uppercase-in-C-Sharp/)