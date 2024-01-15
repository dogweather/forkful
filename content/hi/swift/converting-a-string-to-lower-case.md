---
title:                "स्ट्रिंग को लोअर केस में रूपांतरण करना"
html_title:           "Swift: स्ट्रिंग को लोअर केस में रूपांतरण करना"
simple_title:         "स्ट्रिंग को लोअर केस में रूपांतरण करना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Kyun

Strings ko lower case mein convert karne ka sabse bada karan hai ki ye hamare code mein readability aur consistency ko improve karta hai. Lower case strings ko samjhna aur modify karna aasan hota hai, jisse code ke maintenance aur debugging mein madad milti hai.

## Kaise

String ko lower case mein convert karne ke liye ```lowercased()``` function ka istemal kar sakte hain. Is function ko ```String``` type ke variable ya object par call karke hum uski lower case version bana sakte hain.

```
Swift let name = "JAI HIND"
let lowercasedName = name.lowercased()
print(lowercasedName)
// Output: jai hind
```

Agar hum kisi external source se string receive karte hain (jaise ki user input), toh uss string ko pahle ```uppercased()``` function se upper case mein convert kar sakte hain aur fir ```lowercased()``` function se lower case mein. Isse hum ensure kar sakte hain ki humare code mein sirf lower case strings hi present honge.

```
Swift let userInput = "India"
let lowercasedInput = userInput.uppercased().lowercased()
print(lowercasedInput)
// Output: india
```

## Deep Dive

Swift mein strings ko lower case mein convert karne ke liye character set ka kafi mahatvapurna role hota hai. Character set hume characters ka ek set provide karta hai jo humare string mein present ho sakte hain. Lower case characters ko represent karne ke liye Swift mein "UnicodeScalar" type ka use kiya jata hai.

Har character ke saath uska corresponding lower case UnicodeScalar bhi present hota hai. Yeh UnicodeScalar humare strings ko lower case mein convert karne mein madad karta hai.

## Dekhiye Bhi

1. [Swift Official Documentation on Strings](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
2. [Tutorial on String Handling in Swift](https://www.tutorialspoint.com/swift-programming/swift_strings.htm)
3. [Unicode Scalars in Swift](https://www.hackingwithswift.com/example-code/strings/how-to-convert-a-string-to-a-uniscalar-value-and-back)