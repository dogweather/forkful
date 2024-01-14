---
title:                "Java: 删除与模式匹配的字符"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 为什么要删除匹配模式的字符？

在编程中，有时候我们需要对字符串中的特定字符进行删除操作。这可能是因为这些字符是多余的，也可能是因为它们不符合我们的需求。无论是哪种情况，删除字符是一个常见的操作。在使用Java编程语言时，我们可以使用正则表达式来快速、有效地删除匹配某种模式的字符。接下来，让我们来看一看如何做到这一点。

## 如何使用Java删除匹配模式的字符？

在下面的代码块中，我们将使用Java的String类的replaceAll()方法来删除字符串中的匹配模式的字符。假设我们有一个字符串"Hello World!"，我们想要删除所有的小写字母，只保留大写字母。我们可以使用正则表达式```[a-z]```来匹配所有的小写字母，并将其替换为空字符串。代码如下所示：

```Java
String str = "Hello World!";
str = str.replaceAll("[a-z]", "");
System.out.println(str);
```

输出结果为："HW"

我们可以看到，所有的小写字母都被成功删除了。现在让我们再来看一个稍微复杂一点的例子。假设我们有一个字符串"124abc!&^"，我们想要删除所有的数字和特殊字符，只保留字母。我们可以使用正则表达式```[0-9!&^]```来匹配所有的数字和特殊字符，并将其替换为空字符串。代码如下所示：

```Java
String str = "124abc!&^";
str = str.replaceAll("[0-9!&^]", "");
System.out.println(str);
```

输出结果为："abc"

通过这样的方式，我们可以灵活地使用正则表达式来删除字符串中匹配特定模式的字符。但是请注意，使用正则表达式来进行字符串操作的时候，需要考虑到特殊字符的转义，以免出现意外的结果。

## 深入了解删除匹配模式的字符

在Java中使用replaceAll()方法删除匹配模式的字符时，实际上是创建了一个新的字符串来存储结果。这是因为Java的String类是不可变的，一旦创建就无法修改。因此，虽然看起来是在原始字符串上进行了操作，但实际上是创建了一个新的字符串并将其赋值给原始字符串的引用。这也是为什么我们需要将结果重新赋值给原始字符串。

另外，正则表达式虽然强大，但它的性能并不是最优的。在处理大量字符串时，可能会影响程序的运行效率。因此，在实际的开发中，我们可能需要考虑使用其他方法来删除匹配模式的字符，如使用StringBuilder类来操作字符串，或使用循环结构来逐个检查每个字符。

## 参考资料

- [String类Java文档](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Java正则表达式教程](https://www.runoob.com/java/java-regexp.html)
- [StringBuilder类Java文档](https://docs.oracle.com/javase/8/docs/api/java/lang/StringBuilder.html)

---

## 参考链接

- [Java正则表达式删除非数字和字母的字符](https://www.cnblogs.com/superlee123/p/10200385.html)
- [如何在Java中使用正则表达式进行字符串操作？](https://blog.csdn.net/sdjy1995/article/details/107156602)