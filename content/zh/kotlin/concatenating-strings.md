---
title:    "Kotlin: 连接字符串"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

# 为什么使用Kotlin拼接字符串？

在编程中，经常会需要拼接不同的字符串来创建新的字符串。这可以用于构建动态的文本消息、生成文件名或者拼接数据库查询语句。使用Kotlin的字符串拼接功能可以让这个过程更加简单、高效。 

## 如何拼接字符串 

要拼接字符串，可以使用Kotlin中的`+`运算符或者字符串模板。下面是使用这两种方法的代码示例： 

```
// 使用`+`运算符
val str1 = "Hello"
val str2 = "world"
val str3 = str1 + ", " + str2
println(str3) // 输出结果为 "Hello, world"

// 使用字符串模板
val name = "Alice"
val age = 25
val message = "My name is $name and I am $age years old."
println(message) // 输出结果为 "My name is Alice and I am 25 years old."
```

## 深入探讨字符串拼接

Kotlin中使用字符串拼接功能时，要注意以下几点： 

- `+`运算符可以用于拼接字符串、字符和任意类型的变量，但是使用字符串模板时，变量的类型必须为字符串类型。 

- 当需要拼接大量的字符串时，最好使用`StringBuilder`类。它可以提高性能，因为它会在内部创建一个可变的字符数组来存储字符串，在拼接完成后再将数组转换为字符串。 

- 在Kotlin中，每个字符串都是不可变的。所以每次拼接字符串，都会创建一个新的字符串对象。如果需要频繁拼接字符串，最好使用可变的`StringBuilder`而不是不可变的字符串。 

## 参考链接 

- [Kotlin字符串拼接官方文档（英文版）](https://kotlinlang.org/docs/reference/basic-types.html#strings) 

- [Kotlin字符串拼接教程（中文版）](https://www.kotlincn.net/docs/reference/basic-types.html#strings) 

# 参见 

- [Kotlin字符串提取及检索（中文版）](https://www.kotlincn.net/docs/reference/basic-types.html#string-extractions-and-regex-matches) 

- [Kotlin字符串函数（中文版）](https://www.kotlincn.net/docs/reference/basic-types.html#string-functions)