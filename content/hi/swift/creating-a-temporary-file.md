---
title:                "एक अस्थायी फाइल बनाना"
html_title:           "Swift: एक अस्थायी फाइल बनाना"
simple_title:         "एक अस्थायी फाइल बनाना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Kyu: Ek temporary file banane se hume kya fayda hoga?

Temporary files hume code ko organize karne aur temporary data ko store karne ke liye madad karte hai. Iss tarah se humare code mai clarity bani rehti hai aur hume memory management mai bhi asani hogi.

## Kaise Kare: Temporary File Kaise Banaye

```Swift
let temporaryDirectory = try FileManager.default
    .url(for: .itemReplacementDirectory,
    in: .userDomainMask,
    appropriateFor: URL(fileURLWithPath: "/path/to/file"),
    create: true)
let temporaryFileURL = temporaryDirectory
    .appendingPathComponent("temporaryFile.extension")
```

Iss code mai, hum pehle temporary directory ka path bana kar usse user domain mai store karte hai. Fir uss temporary directory mai ek temporary file bana kar uska extension customize karke store karte hai.

**Output:**

```Temporary file created at path: /Users/username/Library/Containers/com.apple.mail/Data/Library/Mail Downloads/1C7D6B66-B50E-42D1-BDA8-AF98DDADC100/temporaryFile.extension```

## Gehri Jaanch: Temporary File Banane ki Gehri Jaanch

Code mai humne use kiye hai `try` aur `FileManager` ka `default` method. `URL` mai, humne `itemReplacementDirectory` ka option chuna hai, jo temporary files create karne ke liye ideal hai. Fir `userDomainMask` mai humne user ke root directory ko choose kiya hai. Isse temporary file user ke local system mai store hoga aur programmatically delete ho jayega. Ek baar temporary file create ho jane ke baad, hum uski path alag se customize kar sakte hai.

## Dekhiye Bhi:

- [Swift Documentation on FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [Using Temporary Files in Swift](https://www.raywenderlich.com/3687-creating-temporary-files-in-swift)
- [Understanding FileManager in Swift](https://learnappmaking.com/filemanager-swift-how-to-code/)