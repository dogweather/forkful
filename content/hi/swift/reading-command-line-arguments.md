---
title:                "कम्प्यूटर प्रोग्रामिंग पर 'कमांड लाइन आर्ग्यूमेंट्स' का पठन"
html_title:           "Swift: कम्प्यूटर प्रोग्रामिंग पर 'कमांड लाइन आर्ग्यूमेंट्स' का पठन"
simple_title:         "कम्प्यूटर प्रोग्रामिंग पर 'कमांड लाइन आर्ग्यूमेंट्स' का पठन"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Kyun

Agar aap Swift programming language mein command line arguments kaise padh sakte hain, toh ye article aapke liye hai. Ye aapko code ko command line se interact karne ki shakti deta hai aur aapke code ko command line se customize karne mein madad karta hai.

# Kaise Karein

```Swift
// Command line arguments ko access karne ke liye, 'CommandLine' class ka use karein
let arguments = CommandLine.arguments

// Argument count ko print karein
print("Argument count: \(arguments.count)")

// Saare arguments ko loop mein print karein
for argument in arguments {
    print(argument)
}

```

### Output:
```
Argument count: 3
Argument 0: path/to/executable
Argument 1: argument1
Argument 2: argument2
```

# Deep Dive

Command line arguments Swift mein 'CommandLine' class ke through access kiye jaate hain. Is class mein 'arguments' property available hoti hai jo command line arguments ko array format mein store karke rakhti hai. Iske through aap command line se arguments ko read aur manipulate kar sakte hain.

# See Also

- [Swift Programming Language Official Website](https://swift.org/)
- [Swift CommandLine Documentation](https://developer.apple.com/documentation/foundation/commandline)