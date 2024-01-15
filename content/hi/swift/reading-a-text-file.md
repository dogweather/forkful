---
title:                "एक टेक्स्ट फ़ाइल पढ़ना"
html_title:           "Swift: एक टेक्स्ट फ़ाइल पढ़ना"
simple_title:         "एक टेक्स्ट फ़ाइल पढ़ना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Kyun

Kisi bhi programmer ke liye text file padhna ek bahut mahatvapurna kaam hai. Text file se data retrieve karna, manipulate karna aur use karna bahut aasan hota hai. Is article mein hum dekhenge ki Swift mein text file ko kaise padha ja sakta hai.

## Kaise Kare

Text file ko Swift mein padhne ke liye, sabse pehle hum `String` datatype ka use karenge. Is datatype mein hum text file ka content store karenge.

Sabse pehle, hum file URL object banayenge, jis URL pe humara text file stored hai. Iske liye hum `URL` class ka constructor function `init(fileURLWithPath:)` ka use karenge. Is function ko `fileURLWithPath:` ke saath ek file ka path argument dekar hum us file ka URL bana sakte hain. 

```
let fileURL = URL(fileURLWithPath: "/Users/Documents/file.txt")
```

Ab hum file URL se `String` object banayenge, jis mein hum text file ka content store karenge. Iske liye hum `String` class ka `init(contentsOfURL:)` method ka use karenge. Is method ko `contentsOfURL:` ke saath hum file URL object pass kar sakte hain.

```
do {
    let fileContents = try String(contentsOf: fileURL)
    // File ka content `fileContents` mein store hoga
} catch {
    // File ko padhne mein error aaya
    print(error)
}
```

Yeh humara pura code hoga Swift mein text file padhne ka. Agar humne kisi aur file format ka use kiya hai toh hum `encoding:` parameter bhi specify kar sakte hain `String` class ke constructor aur method mein. 

## Deep Dive

Humne dekha ki Swift mein text file ko padhna bahut aasan hai. Agar hume file ke content mein specific character set ka use karna hai, toh hum `encoding:` parameter specify kar sakte hain `String` class ke constructor aur method mein. Isse humare code mein koi encoding related error nahi aayega.

Text file padhne ke alawa, hum `FileManager` class ka use karke bhi file ka content retrieve kar sakte hain. Iske liye hum `contents(atPath:)` method ka use kar sakte hain. Is method mein hume file ka path string pass karna hota hai. 

```
do {
    let fileContentData = try FileManager.default.contents(atPath: "/Users/Documents/file.txt")
    // File ka content binary data format mein hoga `fileContentData` mein store hoga
} catch {
    // File ko padhne mein error aaya
    print(error)
}
```

## See Also

1. [Apple Developer Documentation on Reading and Writing Text Files](https://developer.apple.com/documentation/foundation/file_access_reading_and_writing)
2. [Swift File Management Tutorial](https://www.raywenderlich.com/975/file-management-in-ios-tutorial-getting-started)