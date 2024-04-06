---
date: 2024-01-20 17:53:16.117047-07:00
description: "How to: (\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\
  \u0438:) ."
lastmod: '2024-04-05T21:53:49.995729-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u0412\u0438\u0432\u0435\u0434\u0435\u043D\u043D\u044F \u043D\u0430\u043B\u0430\
  \u0433\u043E\u0434\u0436\u0443\u0432\u0430\u043B\u044C\u043D\u043E\u0457 \u0456\u043D\
  \u0444\u043E\u0440\u043C\u0430\u0446\u0456\u0457"
weight: 33
---

## How to: (Як це зробити:)
```Swift
// Basic output
print("Hello, Ukraine!")

// Interpolating variables
var greeting = "Вітаю"
print("\(greeting), reader!")

// Printing multiple items
let apples = 5
let oranges = 3
print("Apples: \(apples), Oranges: \(oranges)")

// Sample Output:
// Hello, Ukraine!
// Вітаю, reader!
// Apples: 5, Oranges: 3
```

## Deep Dive (Поглиблений Розбір)
Print debugging has been a developer's quick tool since early programming. It's simple yet effective, often helping catch culprits fast. While print statements are handy, use logging tools (like `os_log` in iOS) when you need more control and options. Logging can filter messages, set importance levels, and work better for released apps. Swift also differentiates between print (stdout) and debugPrint (stderr), the latter being more detailed.

## See Also (Дивіться також)
- Swift Documentation on print: https://developer.apple.com/documentation/swift/1541053-print
- Effective logging in Swift: https://www.swiftbysundell.com/articles/logging-in-swift/
- Apple’s Unified Logging System overview: https://developer.apple.com/documentation/os/logging
