---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:26.113268-07:00
description: "\u5982\u4F55\u64CD\u4F5C: Swift\u5BF9\u6B63\u5219\u8868\u8FBE\u5F0F\u7684\
  \u539F\u751F\u652F\u6301\u4F7F\u7528\u4E86`NSRegularExpression`\u7C7B\uFF0C\u4EE5\
  \u53CAString\u7C7B\u7684\u8303\u56F4\u548C\u66FF\u6362\u65B9\u6CD5\u3002\u4E0B\u9762\
  \u662F\u4E00\u4E2A\u4F8B\u5B50\uFF0C\u5C55\u793A\u4E86\u5982\u4F55\u4F7F\u7528\u6B63\
  \u5219\u8868\u8FBE\u5F0F\u5728\u6587\u672C\u5757\u4E2D\u67E5\u627E\u5E76\u9AD8\u4EAE\
  \u7535\u5B50\u90AE\u4EF6\u5730\u5740."
lastmod: '2024-03-13T22:44:48.146593-06:00'
model: gpt-4-0125-preview
summary: "Swift\u5BF9\u6B63\u5219\u8868\u8FBE\u5F0F\u7684\u539F\u751F\u652F\u6301\u4F7F\
  \u7528\u4E86`NSRegularExpression`\u7C7B\uFF0C\u4EE5\u53CAString\u7C7B\u7684\u8303\
  \u56F4\u548C\u66FF\u6362\u65B9\u6CD5\u3002\u4E0B\u9762\u662F\u4E00\u4E2A\u4F8B\u5B50\
  \uFF0C\u5C55\u793A\u4E86\u5982\u4F55\u4F7F\u7528\u6B63\u5219\u8868\u8FBE\u5F0F\u5728\
  \u6587\u672C\u5757\u4E2D\u67E5\u627E\u5E76\u9AD8\u4EAE\u7535\u5B50\u90AE\u4EF6\u5730\
  \u5740."
title: "\u4F7F\u7528\u6B63\u5219\u8868\u8FBE\u5F0F"
weight: 11
---

## 如何操作:
Swift对正则表达式的原生支持使用了`NSRegularExpression`类，以及String类的范围和替换方法。下面是一个例子，展示了如何使用正则表达式在文本块中查找并高亮电子邮件地址:

```swift
import Foundation

let text = "Contact us at support@example.com or feedback@example.org for more information."
let regexPattern = "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}"

do {
    let regex = try NSRegularExpression(pattern: regexPattern)
    let matches = regex.matches(in: text, range: NSRange(text.startIndex..., in: text))

    if !matches.isEmpty {
        for match in matches {
            let range = Range(match.range, in: text)!
            print("Found: \(text[range])")
        }
    } else {
        print("No matches found.")
    }
} catch {
    print("Regex error: \(error.localizedDescription)")
}

// 样例输出:
// Found: support@example.com
// Found: feedback@example.org
```

对于更复杂或注重便利性的场景，你可以使用第三方库，如SwiftRegex，它简化了语法并扩展了可能性。尽管Swift的标准库很强大，一些开发者还是更喜欢这些库，因为它们的语法简洁且有额外的特性。这里是一个使用假想的第三方库执行类似任务的方式:

```swift
// 假设有一个叫做SwiftRegex的库已经被引入
let text = "Reach out at hello@world.com or visit our website."
let emailPattern = "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}"

let emails = text.matches(for: emailPattern) // SwiftRegex提供的假想方法
if emails.isEmpty {
    print("No email addresses found.")
} else {
    emails.forEach { email in
        print("Found: \(email)")
    }
}

// 假想输出，假设`matches(for:)`方法在SwiftRegex中存在:
// Found: hello@world.com
```

这个例子展示了如何使用第三方正则表达式包来简化在字符串内找到匹配项的过程，假设像`matches(for:)`这样的便利方法存在。重要的是要参考相应的第三方库文档以获得准确的语法和方法可用性。
