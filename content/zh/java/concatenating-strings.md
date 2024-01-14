---
title:    "Java: 连接字符串"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/java/concatenating-strings.md"
---

{{< edit_this_page >}}

"## 为什么"

String拼接是Java编程中常见的操作，它可以将多个字符串连在一起，形成一个更长的字符串。为什么要进行字符串拼接呢？这是因为在实际的编程过程中，我们经常需要动态地构建字符串，而不是一次性写好所有内容。例如，当我们需要根据用户输入的不同信息来构建一条完整的消息时，就需要使用字符串拼接功能。

"## 怎么做"

在Java中，我们可以通过使用“+”操作符来进行字符串拼接。让我们来看一个例子：

```Java
String firstName = "小明";
String lastName = "张";
int age = 25;

String message = firstName + lastName + "今年" + age + "岁。";
System.out.println(message);
```

在这个例子中，我们用“+”操作符将三个字符串和一个整数拼接在一起，最终输出的结果是：“小明张今年25岁。”

除了使用“+”操作符，我们还可以使用String类的concat()方法来实现字符串拼接功能。让我们看一个与上面例子功能相同的例子：

```Java
String firstName = "小明";
String lastName = "张";
int age = 25;

String message = firstName.concat(lastName).concat("今年").concat(String.valueOf(age)).concat("岁。");
System.out.println(message);
```

"## 深入了解"

在Java中，字符串是不可变对象，也就是说一旦被创建，其值就无法改变。当我们进行字符串拼接时，实际上是创建了一个新的String对象，而不是在原有的对象上做改变。这也是为什么我们在每次循环中进行字符串拼接时，都会创建新的String对象，而不是直接在原有的字符串上做改变。

另外，由于String对象的不可变性，每次字符串拼接都需要创建新的String对象，这也会带来一定的性能影响。因此，在需要进行大量字符串拼接操作时，建议使用StringBuilder或StringBuffer类，它们都是可变的字符串类，可以提高执行效率。

"## 参考链接"

- [Java String](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Java StringBuilder Class](https://docs.oracle.com/javase/8/docs/api/java/lang/StringBuilder.html)
- [Java StringBuffer Class](https://docs.oracle.com/javase/8/docs/api/java/lang/StringBuffer.html)