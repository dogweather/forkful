---
aliases:
- /he/swift/reading-a-text-file/
date: 2024-01-20 17:55:11.280066-07:00
description: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\
  \u05E1\u05D8 \u05D1-Swift \u05D4\u05D9\u05D0 \u05DC\u05D4\u05E9\u05D9\u05D2 \u05D0\
  \u05EA \u05D4\u05EA\u05D5\u05DB\u05DF \u05DE\u05E7\u05D5\u05D1\u05E5 \u05D5\u05DC\
  \u05E0\u05EA\u05D7\u05D5 \u05D1\u05E7\u05D5\u05D3. \u05E2\u05D5\u05E9\u05D9\u05DD\
  \ \u05D0\u05EA \u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05D8\u05E2\u05D5\u05DF \u05E0\
  \u05EA\u05D5\u05E0\u05D9\u05DD, \u05D4\u05D2\u05D3\u05E8\u05D5\u05EA \u05D0\u05D5\
  \ \u05DC\u05E9\u05DE\u05D5\u05E8 \u05DE\u05D9\u05D3\u05E2 \u05D1\u05D9\u05DF \u05E8\
  \u05D9\u05E6\u05D5\u05EA \u05E9\u05DC \u05D0\u05E4\u05DC\u05D9\u05E7\u05E6\u05D9\
  \u05D4."
lastmod: 2024-02-18 23:08:53.221894
model: gpt-4-1106-preview
summary: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\
  \u05D8 \u05D1-Swift \u05D4\u05D9\u05D0 \u05DC\u05D4\u05E9\u05D9\u05D2 \u05D0\u05EA\
  \ \u05D4\u05EA\u05D5\u05DB\u05DF \u05DE\u05E7\u05D5\u05D1\u05E5 \u05D5\u05DC\u05E0\
  \u05EA\u05D7\u05D5 \u05D1\u05E7\u05D5\u05D3. \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\
  \u05EA \u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05D8\u05E2\u05D5\u05DF \u05E0\u05EA\
  \u05D5\u05E0\u05D9\u05DD, \u05D4\u05D2\u05D3\u05E8\u05D5\u05EA \u05D0\u05D5 \u05DC\
  \u05E9\u05DE\u05D5\u05E8 \u05DE\u05D9\u05D3\u05E2 \u05D1\u05D9\u05DF \u05E8\u05D9\
  \u05E6\u05D5\u05EA \u05E9\u05DC \u05D0\u05E4\u05DC\u05D9\u05E7\u05E6\u05D9\u05D4\
  ."
title: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\
  \u05D8"
---

{{< edit_this_page >}}

## What & Why? (מה ולמה?)
קריאת קובץ טקסט ב-Swift היא להשיג את התוכן מקובץ ולנתחו בקוד. עושים את זה כדי לטעון נתונים, הגדרות או לשמור מידע בין ריצות של אפליקציה.

## How to: (איך לעשות:)
```Swift
import Foundation

func readTextFromFile(fileName: String) -> String? {
    guard let path = Bundle.main.path(forResource: fileName, ofType: "txt") else { return nil }
    
    do {
        let text = try String(contentsOfFile: path, encoding: .utf8)
        return text
    } catch {
        print("Error loading file \(fileName): \(error)")
        return nil
    }
}

if let textContent = readTextFromFile(fileName: "example") {
    print(textContent)
}
```

Sample Output:
```
// The content of example.txt would be printed here.
Hello, Reader!
Welcome to the world of file handling with Swift.
```

## Deep Dive (צלילה עמוקה):
Reading text files is a basic necessity in programming, dating back to the early days of computers. In Swift, we primarily use the `String` class and its `contentsOfFile` initializer to handle this. Alternatives include using `Data` for non-text files or lower-level C APIs for more control. Details like encoding matter; `.utf8` is standard, while others might be used for localization or legacy systems.

## See Also (ראה גם):
- [Swift String Documentation](https://developer.apple.com/documentation/swift/string)
- [Reading and Writing Files - Apple Developer Documentation](https://developer.apple.com/documentation/foundation/filemanager)
- [Swift Book](https://docs.swift.org/swift-book/)
