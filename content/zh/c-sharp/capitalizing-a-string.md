---
title:                "C#: 使用计算机编程：大写一个字符串"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Mandarin Blog Post: 如何在C#中将字符转换为大写

## Why: 为什么要将字符转换为大写？

有时候，在编写C#程序时，您可能需要将字符串中的字符转换为大写。这有助于标准化输入，提高用户体验和使输出更易于阅读。

## How To:

要将字符串转换为大写，您可以使用C#中的 `ToUpper()` 方法。下面是一个示例代码，说明如何使用该方法：

```C#
string str = "hello world";
string upperStr = str.ToUpper();
Console.WriteLine(upperStr);
```

经过此操作后，`upperStr` 的值将为 *HELLO WORLD*。如您所见，`ToUpper()` 方法将所有字符转换为大写形式。

如果您需要将字符串中的特定位置的字符转换为大写，可以使用 `Substring()` 方法和 `ToUpper()` 方法的结合。下面是一个示例代码，演示如何将字符串中的第一个字符转换为大写：

```C#
string str = "hello world";
string firstChar = str.Substring(0, 1);
string upperFirstChar = firstChar.ToUpper();
Console.WriteLine(str.Replace(firstChar, upperFirstChar));
```

此代码输出将为 *Hello world*。

## Deep Dive:

在C#中，可以使用 `ToUpper()` 方法来将字符串转换为大写。但是，实际上该方法并不直接修改原始字符串，而是返回一个新的大写形式的字符串。因此，如果您需要对原始字符串进行更改，您可能需要使用 `ToString()` 方法来覆盖原始字符串。

此外，您还可以使用 `CultureInfo` 对象中的 `TextInfo` 属性来指定特定语言的大小写约定。例如，如果您需要将字符串转换为德语大写形式，可以使用以下代码：

```C#
string str = "hallo welt";
CultureInfo germanCulture = CultureInfo.GetCultureInfo("de-DE");
string upperStr = str.ToUpper(germanCulture.TextInfo);
Console.WriteLine(upperStr);
```

此代码输出将为 *HALLO WELT*。

## See Also:

如果您对字符串操作有兴趣，可以查看以下相关资源：

- [如何在C#中反转字符串](https://www.example.com/reverse-string-C#)
- [C#中常用的字符串方法](https://www.example.com/C#-string-methods)

谢谢阅读！希望这篇文章对您有所帮助。