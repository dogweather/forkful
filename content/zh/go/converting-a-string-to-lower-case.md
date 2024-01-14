---
title:    "Go: 将字符串转换为小写"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 为什么要将字符串转换为小写

在编程中，我们经常需要处理各种各样的字符串。有时候，为了方便比较和操作，我们需要将字符串转换为统一的形式，比如将所有的字母转换为小写。这样就可以避免因为大小写不同而导致的错误。因此，将字符串转换为小写是一个常见的编程操作。

## 如何实现字符串转换为小写

在Go语言中，我们可以使用strings.ToLower()函数来将字符串转换为小写。该函数的语法如下所示：

```Go
strings.ToLower(str)
```

其中，str为需要转换的字符串。下面是一个简单的示例代码，展示如何使用strings.ToLower()函数来将字符串转换为小写：

```Go
str := "HELLO WORLD"
lowercaseStr := strings.ToLower(str)
fmt.Println(lowercaseStr)
```

运行上述代码会输出：hello world，即大写字母被转换为了小写字母。如果字符串中本来就没有大写字母，那么转换后的字符串也不会有变化。

## 深入了解字符串转换为小写

在Go语言中，字符串是不可变的，也就是说，我们无法在原来的字符串上直接进行修改。因此，当我们使用ToLower()函数将字符串转换为小写时，会返回一个新的字符串，原来的字符串并不会改变。这是因为ToLower()函数会创建并返回一个新的字符串，该字符串是原来的字符串的副本，在副本上执行转换操作，从而避免了原来字符串的不可变性带来的问题。

此外，值得一提的是，针对不同的语言环境，ToLower()函数可以实现不同的转换结果。比如在土耳其语环境下，ToLower()函数会将字母"I"转换为"i"，而不是英语中的"i"。因此，在使用ToLower()函数时，我们需要根据具体的语言环境来确定预期的转换结果。

## 然后看看

- [Go语言文档 - strings.ToLower()函数](https://golang.org/pkg/strings/#ToLower)
- [字符串操作的更多方法](https://www.golangprograms.com/go-language/string.html#function)
- [Golang中的变量和类型 - 字符串](https://www.geeksforgeeks.org/golang-strings-package/)