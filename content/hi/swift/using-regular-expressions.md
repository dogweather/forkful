---
title:    "Swift: नियमित अभिव्यक्तियों का उपयोग करना"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## क्यों

यदि आप Swift में कोडिंग करने के लिए एक प्रोग्रामर हैं, तो आपने संभावतः इस शब्द को सुना है - "Regular Expressions"। यह स्ट्रिंग में पैटर्न को ढूंढने और उस पैटर्न के आधार पर स्ट्रिंग को जाँचने का एक अच्छा तरीका है। लेकिन क्या आपको पता है कि आप इसे कैसे प्रयोग कर सकते हैं? इस ब्लॉग पोस्ट में, हम आपको इसके बारे में बताएंगे।

## कैसे करें

इस ब्लॉग पोस्ट में, हम पदों को ढूंढने के लिए एक पैटर्न का उपयोग करने की तकनीक सीखेंगे। साथ ही, हम आपको इसे Swift में कैसे लिखते हैं और कैसे इसका उपयोग करते हैं, भी सिखाएंगे। तो चलिए जानते हैं कि कैसे हम इस शक्तिशाली टूल का उपयोग कर सकते हैं।

आप निम्नलिखित कोड ब्लॉक के माध्यम से एक साधारण Swift फंक्शन का सरल उदाहरण देख सकते हैं।

```Swift
func findMatches(pattern: String, inString string: String) -> [String] {
    var matches = [String]()
    let regex = try! NSRegularExpression(pattern: pattern, options: [])
    let nsString = string as NSString
    regex.enumerateMatches(in: string, options: [], range: NSMakeRange(0, nsString.length)) {
        (match, _, _) in
        if let match = match {
            let matchedString = nsString.substring(with: match.range)
            matches.append(matchedString)
        }
    }
    return matches
}
```

और यदि हम इस फंक्शन को निम्नलिखित अंकित शब्दों के लिए कॉल करेंगे:

```Swift
let string = "मुझे Swift सीखना है। Swift मौजूदा, सबसे लोकप्रिय और अच्छा प्रोग्रामिंग भाषा है।"
let matches = findMatches(pattern: "[A-Za-z]+", inString: string)
```

तो हमारे लिए `मुझे, Swift, सीखना, है, Swift, मौजूदा, सबसे, लोकप्रिय, और, अच्छा, प्रोग्रामिं