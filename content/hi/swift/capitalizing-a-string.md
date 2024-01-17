---
title:                "स्ट्रिंग को कैपिटलाइज़ करना"
html_title:           "Swift: स्ट्रिंग को कैपिटलाइज़ करना"
simple_title:         "स्ट्रिंग को कैपिटलाइज़ करना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
कपिटलाइजंग एक स्ट्रिंग को पहले अक्षर से शुरू करके उसके बाकी चरों को बदलना है। कई बार प्रोग्रामर्स इसे स्ट्रिंग को स्वतंत्र और स्पष्ट रूप में दिखने के लिए करते हैं।

## कैसे करे:
इसके लिए हम आमतौर पर capitalise() या uppercased() को स्ट्रिंग पर कॉल कर सकते हैं। नीचे दिए गए कोड ब्लॉक में यह दिखाया गया है:

```
let str = "hello world"
print(str.capitalized) // आउटपुट: Hello World
print(str.uppercased) // आउटपुट: HELLO WORLD
```

## गहराई में जाएं:
कपिटलाइजंग का इतिहास बहुत पुराना है, और एक समय पर यह फॉर्मल आउटपुट की उस स्टाइल में दिखाने के लिए किया जाता था। अन्य विकल्प शामिल हैं:

- capitalize() और uppercased() के अलावा, हम lowercased() भी का उपयोग अनुभव कर सकते हैं। यह स्ट्रिंग को स्वतंत्र अक्षर के साथ दिखाता है।
- गणना ध्यान देने योग्य है कि अक्षर कैसे करके स्ट्रिंग को नीवन किया जाता है। प्रत्येक अल्फान्यूमेरिक मूल्य की क्षमता प्रभावी रूप से कैपिटलाइज हो जाता है।

## देखें भी:
- [Swift की मूल स्ट्रिंग डॉक्यूमेंटेशन](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Hacking with Swift की "Working with strings" ट्यूटोरियल](https://www.hackingwithswift.com/sixty/2/3/working-with-strings)
- [Swift की स्ट्रिंग रीफरेंस गाइड](https://developer.apple.com/documentation/swift/string)