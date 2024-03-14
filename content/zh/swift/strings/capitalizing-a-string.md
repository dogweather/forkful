---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:31.175361-07:00
description: "\u5728Swift\u4E2D\u5927\u5199\u5316\u4E00\u4E2A\u5B57\u7B26\u4E32\u610F\
  \u5473\u7740\u5C06\u7ED9\u5B9A\u5B57\u7B26\u4E32\u7684\u7B2C\u4E00\u4E2A\u5B57\u7B26\
  \u8F6C\u6362\u4E3A\u5927\u5199\uFF0C\u5176\u4F59\u5B57\u7B26\u8F6C\u6362\u4E3A\u5C0F\
  \u5199\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u6309\u7167\u8BED\
  \u6CD5\u89C4\u5219\u6216\u7528\u6237\u754C\u9762\u6807\u51C6\u683C\u5F0F\u5316\u540D\
  \u79F0\u6216\u53E5\u5B50\u3002"
lastmod: '2024-03-13T22:44:48.138850-06:00'
model: gpt-4-0125-preview
summary: "\u5728Swift\u4E2D\u5927\u5199\u5316\u4E00\u4E2A\u5B57\u7B26\u4E32\u610F\u5473\
  \u7740\u5C06\u7ED9\u5B9A\u5B57\u7B26\u4E32\u7684\u7B2C\u4E00\u4E2A\u5B57\u7B26\u8F6C\
  \u6362\u4E3A\u5927\u5199\uFF0C\u5176\u4F59\u5B57\u7B26\u8F6C\u6362\u4E3A\u5C0F\u5199\
  \u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u6309\u7167\u8BED\u6CD5\
  \u89C4\u5219\u6216\u7528\u6237\u754C\u9762\u6807\u51C6\u683C\u5F0F\u5316\u540D\u79F0\
  \u6216\u53E5\u5B50\u3002"
title: "\u5B57\u7B26\u4E32\u5927\u5199\u5316"
---

{{< edit_this_page >}}

## 什么 & 为什么？

在Swift中大写化一个字符串意味着将给定字符串的第一个字符转换为大写，其余字符转换为小写。程序员这样做是为了按照语法规则或用户界面标准格式化名称或句子。

## 如何操作：

Swift的`String`结构体提供了一些内置方法来操作字符串的大小写。这里有一些在Swift中大写化字符串的方法，包括使用标准方法和必要时使用第三方库。

### 使用内置方法

要将字符串的第一个字母大写并将其余字母小写：

```swift
let myString = "hello, world"
let capitalizedString = myString.prefix(1).uppercased() + myString.dropFirst().lowercased()
print(capitalizedString) // 输出："Hello, world"
```

要将句子中每个单词的第一个字母大写，您可以使用`capitalized`属性：

```swift
let sentence = "hello, world"
let capitalizedSentence = sentence.capitalized
print(capitalizedSentence) // 输出："Hello, World"
```

### 使用第三方库

虽然Swift的标准库相当全面，但一些特定的大小写格式可能需要更复杂的操作，或者可以使用第三方库来简化。用于字符串操作的其中一个受欢迎的库是SwiftRichString。（注意：始终通过Swift包管理器，CocoaPods或Carthage来包含第三方库，并在文件中导入它们。）

首先，您需要将`SwiftRichString`添加到您的项目中。一旦安装，您就可以使用它来进行各种字符串操作，包括特定的大小写需求。然而，截至目前，Swift的内置方法已经足够覆盖大多数大小写使用场景，而不需要外部库仅用于大写化字符串。

始终参考该库的最新文档以获取任何更新或方法变更。
