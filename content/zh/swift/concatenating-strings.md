---
title:                "Swift: 连接字符串"
simple_title:         "连接字符串"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

为什么：字串拼接是编程中一个非常常用的任务，可以将不同的信息结合起来，使其更有用处。

如何完成：在Swift中，您可以使用加号运算符手动将两个字符串拼接起来，或者使用String类中的append（）方法。下面是一个例子：

```
Swift let firstName = "张三"
let lastName = "李四"
let fullName = firstName + lastName
print（fullName）

// Output：张三李四
```

您也可以使用String类中的```+=```运算符来实现相同的效果。此外，还可以使用插值字符串将变量或常量的值添加到现有字符串中，例如：

```
Swift let age = 26
let nameAndAge = "我的名字是 \(fullName) ，我今年 \(age)岁。"
print（nameAndAge）

// Output：我的名字是 张三李四 ，我今年26岁。
```

深入了解：当我们使用加号运算符或```append（）```方法来拼接字符串时，实际上是在创建一个新的字符串，并将原始字符串和要拼接的字符串连接起来。这意味着每次拼接字符串时都会创建新的内存空间，这可能会影响程序的性能。为了避免这种情况，我们可以使用Swift中的```StringInterpolation```功能来更有效地拼接字符串。这种方法允许我们在同一行中将多个字符串和值连接起来，如下所示：

```
Swift let message = "您好，我的名字是\(fullName)，我今年\(age)岁。我最喜欢的数字是\(favoriteNumber)。"
print（message）

// Output：您好，我的名字是张三李四，我今年26岁。我最喜欢的数字是8。
```

请注意，这种方法只适用于Swift4及更高版本。

另外，我们还可以使用```join（）```方法来拼接字符串数组，如下所示：

```
Swift let wordsArray = ["我", "是", "一个", "程序员"]
let sentence = wordsArray.join（separator：''）
print（句子）

//Output：我是一个程序员
```

此外，Swift还提供了一些其他用于字符串操作的强大功能，例如替换子字符串，将字符串转换为大写或小写，检查字符串是否包含特定字符等。您可以查看下面的链接来了解更多关于字符串操作的信息。

参见：

- [关于Swift字符串的官方文档](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [使用Swift字符串的实用例子](https://www.hackingwithswift.com/example-code/strings/how-to-split-a-string-into-an-array)
- [Swift中字符串操作的强大功能](https://www.hackingwithswift.com/articles/181/how-to-use-strings-in-swift)