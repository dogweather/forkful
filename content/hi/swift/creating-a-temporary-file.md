---
title:                "Swift: कॉम्यूटर प्रोग्रामिंग पर एक लेख का शीर्षक: एक अस्थायी फ़ाइल बनाना"
simple_title:         "कॉम्यूटर प्रोग्रामिंग पर एक लेख का शीर्षक: एक अस्थायी फ़ाइल बनाना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Kyu
Temporary file bana kar log aksar apne Swift code mein use karte hai taki kisi temporary data ya temporary storage ko handle kar sake. Temporary file bana kar sath hi unhe delete bhi kar sakte hai.

## Kaise Karein
Temporary file banana bahut hi aasan hai Swift mein. Aapko bas ek temporary file identifier create karna hai jise aap file ke through access kar sakte hai. Uske baad aap us identifier ka use karke file create kar sakte hai.

```Swift
let fileManager = FileManager.default
let tempDirectoryURL = URL(fileURLWithPath: NSTemporaryDirectory(), isDirectory: true)
let tempFileURL = tempDirectoryURL.appendingPathComponent("mytempfile.txt") // temporary file name
fileManager.createFile(atPath: tempFileURL.path, contents: nil)
```

Ab aap temporary file ko use kar sakte hai jaise normal file ko use karte hai. Aur jab aapko temporary file ko delete karna hota hai, tab aap identifier ka use karke file ko delete kar sakte hai.

```Swift
let fileManager = FileManager.default
let tempDirectoryURL = URL(fileURLWithPath: NSTemporaryDirectory(), isDirectory: true)
let tempFileURL = tempDirectoryURL.appendingPathComponent("mytempfile.txt")
do {
    try fileManager.removeItem(at: tempFileURL)
} catch {
    print("Error deleting temporary file: \(error)")
}
```

## Deep Dive
Swift mein temporary file create karna temp directory ki help se hota hai. Temp directory ko NSTemporaryDirectory() function se access kiya ja sakta hai. Ye function temporary file ko create karne ke liye recommended hai kyunki ye temporary file ko system cache se related data ko handle aur management karne mein help karta hai.

Temporary file ko delete karne ke liye bhi hum NSTemporaryDirectory() function ka use kar sakte hai jis se temporary file ko directly temporary directory se delete kiya ja sakta hai.

# See Also
- [NSFileManager Class Reference](https://developer.apple.com/documentation/foundation/nsfilemanager)
- [NSTemporaryDirectory Function Documentation](https://developer.apple.com/documentation/foundation/1409181-nstemporarydirectory)