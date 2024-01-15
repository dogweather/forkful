---
title:                "连接字符串"
html_title:           "C#: 连接字符串"
simple_title:         "连接字符串"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

为什么:

如果你想要在程序中把多个字符串连接在一起，那么你需要了解如何使用C#中的字符串连接功能。

如何进行字符串连接:

```C#
// 创建两个字符串
string str1 = "Hello";
string str2 = "World";

// 使用"+"操作符连接字符串
string result = str1 + str2;

// 输出结果为 "HelloWorld"
Console.WriteLine(result);
```

除了使用"+"操作符，C#还有另外两种方法可以连接字符串。第一种是使用String类中的Concat()方法，代码如下：

```C#
// 创建两个字符串
string str1 = "Hello";
string str2 = "World";

// 使用Concat()方法连接字符串
string result = String.Concat(str1, str2);

// 输出结果为 "HelloWorld"
Console.WriteLine(result);
```

第二种方法是使用String类中的Join()方法，代码如下：

```C#
// 创建一个字符串数组
string[] words = {"Hello", "World", "from", "C#"};

// 使用Join()方法连接数组中的所有字符串，以空格作为分隔符
string result = String.Join(" ", words);

// 输出结果为 "Hello World from C#"
Console.WriteLine(result);
```

深入了解:

使用"+"操作符连接字符串的背后，其实是使用了String类中的Concat()方法。而Concat()方法则是通过创建一个新的String对象，将两个字符串拼接在一起，然后返回该对象。这就意味着每次使用"+"操作符连接字符串时，都会创建一个新的String对象，这在一些复杂的程序中可能会降低性能。而使用String类中的Concat()方法则不会创建新的对象，因此在性能方面有一定的优势。

另外，在C#中使用Concat()方法和Join()方法连接字符串时，还可以指定用于分隔字符串的字符或字符串，从而实现更多样化的字符串连接方式。

# 参考资料:

1. [Microsoft Docs - String Concatenation](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/?#concatenation)
2. [C-SharpCorner - String Concatenation in C#](https://www.c-sharpcorner.com/UploadFile/eb0c69/concatenation-in-C-Sharp/)
3. [GeeksforGeeks - C# | String Concatenation](https://www.geeksforgeeks.org/c-sharp-string-concatenation/)
4. [C#教程 - 字符串连接](https://www.runoob.com/dotnet/csharp-string-concat.html)

## 看看这里

- [String Concatenation 操作符](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/operators/addition-operator#string-concatenation-operator-)
- [String Concatenation 操作符 vs Concat() 方法](https://stackoverflow.com/questions/1516550/string-concatenation-vs-stringbuilder-which-is-lots-slower)