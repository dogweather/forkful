---
title:    "C#: 提取子字符串"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## 为什么

在编写C#程序时，经常会遇到需要提取字符串中的一部分字符的情况。例如，从一个长字符串中提取出某个单词或者日期。使用提取子字符串的技巧可以帮助我们更轻松地处理这些需求。

## 如何做

提取子字符串是一个比较常见的操作，C#提供了多种方法来实现这个功能。下面我们将介绍三种常用的方法，通过实例演示具体的代码和输出。

### 方法一：Substring()

Substring()方法可以从一个字符串中提取出指定的子字符串。它的语法如下：

```
string.Substring(startIndex, length); 
```

示例代码：

```
string str = "Hello World";
string subStr = str.Substring(6, 5);
Console.WriteLine(subStr);
```

输出结果为：

```
World
```

### 方法二：Split()

Split()方法可以把一个字符串按照指定的分隔符分割成字符串数组，并选择需要的子字符串。它的语法如下：

```
string[] strArr = string.Split(separators, options); 
```

示例代码：

```
string str = "Hello World";
string[] words = str.Split(' ');
string subStr = words[1];
Console.WriteLine(subStr);
```

输出结果为：

```
World
```

### 方法三：Regex.Match()

如果字符串中包含了特定的模式，可以使用正则表达式来提取子字符串。Regex.Match()方法可以根据指定的正则表达式从字符串中提取匹配的子字符串。它的语法如下：

```
string subStr = Regex.Match(input, pattern).Value; 
```

示例代码：

```
string str = "Today is 2020-07-01.";
string subStr = Regex.Match(str, @"\d{4}-\d{2}-\d{2}").Value;
Console.WriteLine(subStr);
```

输出结果为：

```
2020-07-01
```

## 深入探讨

除了上述提到的方法之外，C#还提供了很多其他的方式来提取子字符串。例如Substring()方法还可以直接通过指定结束索引来提取子字符串，而不必指定长度。另外，如果需要忽略大小写来匹配子字符串，可以使用字符串的ToLower()方法或者正则表达式的IgnoreCase选项。

## 参考链接

- [C#中字符串的常用操作](https://www.runoob.com/csharp/csharp-string.html)
- [C#字符串的实现、for循环和Substring()的性能比较](https://blog.zhaojie.me/2014/11/csharp-substring-performance.html)
- [正则表达式匹配](https://www.runoob.com/csharp/csharp-regular-expressions.html)