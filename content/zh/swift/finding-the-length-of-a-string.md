---
title:                "获取字符串的长度"
date:                  2024-01-20T17:48:35.885077-07:00
model:                 gpt-4-1106-preview
simple_title:         "获取字符串的长度"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why? 什么 & 为什么？
字符串长度，就是字符的总数。程序员需要了解长度来处理文本数据，比如验证用户输入、设置界面元素或进行文本切割。

## How to 使用方法
Swift 中，计算字符串长度非常直接。使用 `count` 属性就能搞定。

```Swift
let greeting = "你好，世界！"
print(greeting.count)  // 输出字符串长度
```

输出结果：

```
6
```

注意：这里的“字符”是指图形字符，也就是包括组合而成的字符。

## Deep Dive 深入了解
在早期的 Swift 版本中，字符串的索引不那么直观，因为 Swift 的字符串是基于 Unicode 标量构建的。`count` 属性背后，Swift 在处理例如组合字符序列这样的复杂情况，所以它不总是返回预期的字符数。

例如，表情符号和某些语言字符会被计为单一个字符，即使它们实际由多个 Unicode 标量组合而成。这是因为 Swift 的 String 类型是 Unicode 合规的，计算字符长度考虑了 Unicode 的全球通用特性。

除了 `count`，我们也可以使用其他方法进行字符串长度的检查，比如 `utf16.count` 或者 `utf8.count`，这将分别以 UTF-16 和 UTF-8 的方式量度字符串长度。这对于网络传输或文件存储的编码层面尤为重要。

```Swift
let flag = "🇨🇳"

print(flag.count)          // Unicode 标量长度
print(flag.utf16.count)    // UTF-16 编码长度
print(flag.utf8.count)     // UTF-8 编码长度
```

输出结果：

```
1
4
6
```

这个例子说明了，字符串的长度可能依据编码有所不同。

## See Also 另请参阅
- Swift 官方文档中的 Strings and Characters（[链接](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)）
- Unicode 标准（[链接](https://www.unicode.org/standard/standard.html)）
- Swift String Manifesto（[链接](https://github.com/apple/swift/blob/main/docs/StringManifesto.md)），对 Swift 字符串设计哲学的深入探讨。