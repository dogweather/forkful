---
title:                "Swift: स्ट्रिंग्स को जोड़ना"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## क्यों

स्ट्रिंग को जोड़ने में इंजीनियरिंग होती है क्योंकि यह एक स्ट्रिंग के असामान्य संख्या जो तीसरे पक्ष के पर्यायों के साथ भिन्न होने की संभावना है। स्विफ्ट प्रोग्रामिंग भाषा एक संकल्प पर आधारित भाषा है जिसमें आप किसी भी हिस्से को संख्या बना सकते हैं। आप इस कारण से संग्रहित स्ट्रिंग का स्वतंत्र घोषणा स्ट्रिंग के साथ भिन्न नहीं हो सकते हैं

## कैसे करें

```Swift
let firstName = "विश्वपवन"
let lastName = "कुमार"
let fullName = firstName + " " + lastName

print(fullName) // विश्वपवन कुमार
```

## गहराई में जाएं

स्ट्रिंग को जोड़ने के लिए, हम एक अलग स्ट्रिंग को दूसरे स्ट्रिंग के साथ जोड़ सकते हैं, जिसमें हम जोड़ने के साथ एक ‘एकत्रित’ ऑपरेटर का उपयोग करते हैं। यहां, हमारे पास दो स्ट्रिंग हैं जो एक साथ जोड़ने का स्वरूप बनते हैं। इस तरह से, हम आप उन्हें एक साथ जोड़ सकते हैं और आपका नतीजे में नया स्ट्रिंग विश्लेषण करेंगे। नया स्ट्रिंग अनुत्पाद स्विफ्ट प्रोग्रामिंग के लिए तैयार हो जाएगा।

## देखो की

ऐसा करने के लिए अधिक उदाहरण हासिल करने के लिए, नीचे दी गई लिंक्स की जांच करें:

- [Swift Strings and Characters - Apple Documentation](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Concatenating strings in Swift - Paul Hudson](https://www.hackingwithswift.com/example-code/strings/how-to-concatenate-strings-in-swift)
- [Concatenating String and Characters - Kunal Kapadia](https://kunalkapadia.github.io/swift/strings/characters/2016/02/14/concatenating-strings-and-characters-in-swift.html)