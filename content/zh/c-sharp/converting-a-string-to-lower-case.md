---
title:    "C#: 转换字符串为小写"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

为什么：将字符串转换为小写的原因

字符串转换为小写在C#编程中非常常见，它可以帮助程序员更容易地处理输入的文本。通过将文字全部转换为小写，我们可以消除大小写的差异，从而使比较和搜索更加简单。

代码段1：
```C#
string sample = "Hello, World!";
string lowerCase = sample.ToLower();
Console.WriteLine(lowerCase);
// 输出："hello, world!"
```

代码段2：
```C#
string name = "Jenny";
name = name.ToLower();
if (name == "jenny") 
{
    Console.WriteLine("Hello, Jenny!");
}
// 输出："Hello, Jenny!"
```

代码段3：
```C#
string input = Console.ReadLine();
string lowerInput = input.ToLower();
// input 和 lowerInput 可以通过字符串比较确认是否相同
```

深入介绍：

字符串转换为小写有时候也被称为规范化，因为它会将所有的文本变为统一的格式。这在处理用户输入时非常有用，因为用户可能会使用不同的大小写来输入同一个单词。另外，在比较文本时，转换为小写可以忽略大小写的差异，从而提高了效率。

另外，如果要将输入的文本与存储在数据库中的文本进行比较，转换为小写也是必要的。因为数据库中存储的常常都是小写的文本，如果我们的输入与之不同，就会导致比较失败。

字符串转换为小写不仅仅适用于字母，还适用于其他语言中的音标和特殊字符。它可以将Unicode字符都转换为小写形式，从而保持文本的完整性。

另外需要注意的是，字符串转换为小写是不可逆的，即使使用了两次ToUpper()方法也无法恢复原来的字符串。所以在进行转换时，务必要十分小心。

另一个需要注意的点是，在一些语言中，汉字是没有大小写之分的，所以如果我们要将汉字转换成小写形式，实际上并没有起到实际的作用。在这种情况下，我们可以考虑使用其他的方法来处理文本。

总的来说，字符串转换为小写是一种常用的编程技巧，它可以帮助我们更轻松地处理文本，使得程序更加健壮和易于维护。通过掌握这个方法，我们可以更加灵活地处理文本，并且提升程序的效率。

参考链接：
- [字符串操作](https://docs.microsoft.com/zh-cn/dotnet/csharp/programming-guide/strings/)
- [字符串大小写转换](https://www.tutorialsteacher.com/csharp/csharp-string-to-method)
- [规范化文本](https://en.wikipedia.org/wiki/Normalization_(Unicode))