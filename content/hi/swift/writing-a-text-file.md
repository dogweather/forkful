---
title:                "टेक्स्ट फाइल लिखना"
date:                  2024-01-19
html_title:           "Bash: टेक्स्ट फाइल लिखना"
simple_title:         "टेक्स्ट फाइल लिखना"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

Text file लिखने का मतलब है किसी डेटा को plain text में सेव करना। Programmers इसे configuration, data storage, या logging के लिए करते हैं।

## How to: (कैसे करें:)

Swift में text file लिखने के लिए `String` की `write()` method का उपयोग होता है। यहाँ उदाहरण दिया गया है:

```Swift
import Foundation

let str = "नमस्ते! Swift में फाइल लिखना सीख रहे हैं।"
let fileURL = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("Example.txt")

do {
    try str.write(to: fileURL, atomically: true, encoding: .utf8)
    print("फाइल लिखने में सफलता!")
} catch {
    print("फाइल लिखने में असफल: \(error)")
}
```

## Deep Dive (गहन जानकारी):

पहले programmers ने C जैसी भाषाओं में file handling functions का इस्तेमाल किया। Swift में `String` की `write()` method आसान है, मगर `OutputStream` और `FileHandle` जैसे alternatives भी मौजूद हैं, जो stream आधारित लेखन या बड़ी files के लिए मुफीद हैं। Implementation में atomic लिखना सुनिश्चित करता है कि डेटा fully written होने तक original content corrupt नहीं होता।

## See Also (और देखें):

- Swift Documentation: [Writing Strings to a File](https://swift.org/documentation/)
