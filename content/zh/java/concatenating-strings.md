---
title:    "Java: 字符串连接"
keywords: ["Java"]
---

{{< edit_this_page >}}

## 为什么

在编写Java程序时，经常会遇到需要将多个字符串连接在一起的情况。这可以通过使用“+”运算符或者concat()方法来实现。经过连接后，这些字符串就可以作为一个整体来使用，方便在一些特定情况下操作。

## 怎么做

使用“+”运算符连接字符串：

```Java
String firstName = "小明";
String lastName = "张";
String fullName = firstName + lastName;

System.out.println(fullName);
```

输出结果为：

```
小明张
```

使用concat()方法连接字符串：

```Java
String title = "您好";
String content = "，欢迎来到我的博客！";
String message = title.concat(content);

System.out.println(message);
```

输出结果为：

```
您好，欢迎来到我的博客！
```

## 深入了解

在Java中，字符串是不可变的，即不能直接修改一个已存在的字符串。所以当我们需要对一个字符串进行拼接时，其实是创建了一个新的字符串。这样设计的好处是可以提高字符串的效率，因为不需要频繁修改原有字符串的内容。但同时也需要注意，如果在一个循环中频繁拼接字符串，会产生大量临时对象，可能导致内存占用过多。

另外，在使用“+”运算符连接字符串时，编译器会自动优化为使用StringBuilder或StringBuffer来实现，这两者相比于直接使用“+”运算符，性能更高。如果需要频繁拼接字符串，建议使用StringBuilder或StringBuffer来代替。

## 参考资料

- [Java字符串连接介绍](https://www.runoob.com/java/java-string-class.html)
- [Java运算符详解](https://www.runoob.com/java/java-operators.html)
- [String类API文档](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)

## 另请参阅

- [Java字符串拼接性能分析](https://www.jianshu.com/p/dec285742847)
- [StringBuilder和StringBuffer的区别](https://blog.csdn.net/u014704249/article/details/79196260)