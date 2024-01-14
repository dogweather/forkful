---
title:    "C#: 将字符串大写"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

为什么：为什么会有人对字符串进行大写？只需要1-2句话来解释这个问题。

如何：包含"```C# ... ```"代码块的编码示例和样本输出。

深入了解：有关字符串大写的更多信息。

在编程中，字符串是一个常见的数据类型，它由一系列的字符组成。有时候，我们需要对字符串进行操作，例如将其转换为大写或小写，用于比较、显示等等。今天，我们就来深入了解如何在C#中将字符串转换为大写。

### 为什么

有时候，我们需要对字符串进行操作来满足特定的需求。可能是要进行比较、显示、或者是将单词首字母大写，以符合语言规范。无论是什么原因，对字符串进行大写操作都是很常见的需求。

### 如何

要在C#中对字符串进行大写，我们需要使用内置的ToUpper()方法。下面是一个简单的示例代码：

```C#
string name = "hello world";
string upperName = name.ToUpper(); // HELLO WORLD
```

在这个示例中，我们将字符串"name"赋值为"hello world"，然后使用ToUpper()方法将其转换为大写，并将结果赋值给"upperName"变量。现在，"upperName"变量中的字符串就是大写形式的"HELLO WORLD"。

除了单纯的转换为大写，ToUpper()方法还可以接收CultureInfo参数，用于指定特定的地区语言规范。这样可以确保转换后的结果符合特定地区的语言习惯。下面是一个带有CultureInfo参数的示例代码：

```C#
string name = "hello world";
string upperName = name.ToUpper(new CultureInfo("en-US")); // HELLO WORLD
```

在这个示例中，我们使用了"en-US"的CultureInfo参数，将字符串转换为美国英语的大写形式。

除了使用ToUpper()方法外，还可以使用String类中的ToUpperInvariant()方法，在进行比较时，这种方式更安全。

### 深入了解

C#中的字符串有不可变性，即一旦创建就不能更改。因此，在对字符串进行大写操作时，并不是直接在原有的字符串上进行修改，而是创建一个新的字符串并返回。这就意味着，如果在程序中多次对同一个字符串进行大写操作，会导致不必要的内存开销。

为了解决这个问题，可以使用StringBuilder类来代替String类。StringBuilder类允许我们在原有的字符串上进行修改，并不会创建新的字符串。下面是使用StringBuilder类来进行大写操作的示例代码：

```C#
string name = "hello world";
StringBuilder sb = new StringBuilder(name);
sb.ToUpper();
string upperName = sb.ToString(); // HELLO WORLD
```

在这个示例中，我们创建了一个StringBuilder对象"sb"，并将原有的字符串作为参数传入。然后使用ToUpper()方法对StringBuilder对象进行操作，最后将结果转换为字符串并赋值给"upperName"变量。这样就可以避免多次创建新的字符串的情况。

### 查看更多

了解如何在C#中对字符串进行大小写转换并不是一个难题。但是在实际的开发中，我们可能会遇到更复杂的需求。如果想深入了解字符串的更多操作和用法，可以查看下面的参考链接。

[Microsoft Docs - String.ToUpper Method](https://docs.microsoft.com/en-us/dotnet/api/system.string.toupper?view=net-5.0)

[Microsoft Docs - StringBuilder Class](https://docs.microsoft.com/en-us/dotnet/api/system.text.stringbuilder?view=net-5.0)

[Microsoft Docs - CultureInfo Class](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo?view=net-5