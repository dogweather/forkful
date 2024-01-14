---
title:    "Java: 计算字符串的长度"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# 为什么

当我们处理文本数据时，经常会需要知道字符串的长度。例如，我们想要限制用户输入的文本长度，或者我们需要统计一个句子中有多少个单词。在这些情况下，就需要用到字符串的长度。

# 怎么做

要获取一个字符串的长度，在Java中有一个内置的方法`length()`可以使用。下面是一个简单的示例代码：

```Java
String str = "你好世界！";
int length = str.length();
System.out.println(length);
// Output: 5
```

首先，我们定义一个包含字符串的变量`str`，然后使用`length()`方法获取它的长度。最后，我们将结果打印出来。这里要注意的是，虽然这个字符串是中文，但是Java会将每个中文字符都认为是一个字符，所以长度为5，而不是3。

除了普通的字符串，我们还可以获取字符数组的长度。字符串实际上也可以看作是一个字符数组，因此也可以使用`length()`方法来获取字符串的长度。

```Java
char[] chars = {'H', 'e', 'l', 'l', 'o'};
String str = new String(chars);
System.out.println(str.length());
// Output: 5
```

# 深入了解

在Java中，字符串的长度实际上是存储在一个叫做`length`的成员变量中的。每当我们调用`length()`方法时，实际上是在获取这个成员变量的值。这样做可以提高性能，因为每次调用都不需要重新计算字符串的长度。

另外，有时候我们可能会用空格或者其他特殊字符来填充一个字符串，这样就会影响实际的字符数。在这种情况下，我们可以使用`trim()`方法来删除字符串的开头和结尾的空格，然后再获取长度。

```Java
String str = "   Hello   ";
System.out.println(str.trim().length());
// Output: 5
```

# 参考链接

- [官方文档 - String类](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#length--)
- [Java字符串：长度（length）的重要性](https://www.itread01.com/content/1541112606.html)
- [Java中获取字符串长度的三种方式](https://www.cnblogs.com/interdrp/tag/Java%E4%B8%AD%E8%8E%B7%E5%8F%96%E5%AD%97%E7%AC%A6%E4%B8%B2%E9%95%BF%E5%BA%A6/)