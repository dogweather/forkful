---
title:                "Swift: 将字符串转换为大写"
simple_title:         "将字符串转换为大写"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# 为什么

在编写Swift代码时，有时我们需要仅对字符串中的首字母大写，而不改变其余字母的大小写。这可以用来改变字符串的显示效果，或者用来格式化用户输入的文本。下面介绍如何在Swift中实现字符串的首字母大写。

# 怎么做

我们可以使用Swift中的一个内置函数来实现字符串的首字母大写，即`.capitalized`方法。下面是一个简单的示例，展示如何使用该方法来对输入的字符串进行首字母大写的操作：

```
// 创建一个字符串变量
var str = "hello, world!"

// 对字符串进行首字母大写
let capitalizedStr = str.capitalized

// 输出结果为"Hello, World!"
print(capitalizedStr)
```

除了针对整个字符串使用`.capitalized`方法，我们也可以对每个单词的首字母进行大写操作。这可以通过使用`.capitalized`方法的另一个参数来实现，即`.words`，它会将字符串拆分成单词数组，并对每个单词的首字母进行大写操作。下面是一个示例代码：

```
// 创建一个字符串变量
var str = "hello, world!"

// 对字符串进行首字母大写，但只针对单词首字母
let capitalizedStr = str.capitalized(with: .words)

// 输出结果为"Hello, World!"
print(capitalizedStr)
```

# 细节深入

在Swift中，字符串是不可变的，这意味着我们不能直接修改字符串的内容。因此，当我们使用`.capitalized`方法时，实际上是创建了一个新的字符串，并对其进行首字母大写的操作。原始的字符串保持不变。

另外，`.capitalized`方法并不仅仅适用于英文字符串，它也可以处理其他语言的字符串，如中文、日文等。它会根据每个语言的首字母规则来进行大写操作。这使得在国际化的应用中，我们可以轻松地实现字符串的首字母大写功能。

# 参考链接

- [Swift API文档](https://developer.apple.com/documentation/swift/string/capitalized)
- [字符串操作指南](https://www.hackingwithswift.com/quick-start/understanding-swift/when-should-you-use-strings-in-swift)
- [字符串的不可变性](https://www.geeksforgeeks.org/immutable-strings-in-swift/)
- [字符串国际化指南](https://developer.apple.com/documentation/foundation/bundle/1520698-preferredlocalizations)
- [字符串本身的操作方法](https://www.avanderlee.com/swift/string-operations-swift/)

# 参见

- [如何在Swift中对字符串进行其他操作？](https://example.com/other-string-operations)
- [Swift中如何处理国际化字符串？](https://example.com/localized-strings-swift)