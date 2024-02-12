---
title:                "Виведення налагоджувальної інформації"
aliases:
- /uk/swift/printing-debug-output/
date:                  2024-01-20T17:53:16.117047-07:00
model:                 gpt-4-1106-preview
simple_title:         "Виведення налагоджувальної інформації"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why? (Що і Чому?)
Printing debug output is displaying temporary messages in the console to check if the code works right. Programmers do it to track variables, understand the flow, and catch bugs.

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
