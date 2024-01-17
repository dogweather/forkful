---
title:                "उपस्तंभ निकालना"
html_title:           "Swift: उपस्तंभ निकालना"
simple_title:         "उपस्तंभ निकालना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

Substring निकालना क्या है और programmers इसे क्यों करते हैं, यह दोनों ही महत्वपूर्ण सवाल हैं। Substring, किसी भी string से छोटे हिस्से को निकालना होता है जो उस string के भीतर होता है। Programmers substring का इस्तेमाल अक्सर string में से निर्दिष्ट जानकारी को निकालने और उसे उपयोगी तरीके से प्रदर्शित करने के लिए करते हैं।

## कैसे करें?

`Swift` में substring निकालने का सबसे आसान और प्रभावी तरीका `substring(from: )` और `substring(to: )` है। दूसरे स्थानों पर, `substring(from: )` से आप किसी भी string में से दिया गया starting index से शुरू होकर उपलब्ध character तक का substring निकाल सकते हो। वहीं, `substring(to: )` से आप किसी भी string में से शुरू से लेकर दिए गए ending index तक का substring निकाल सकते हो। नीचे कुछ coding उदाहरण हैं:

```Swift
let name = "John Doe"

// substring(from: )
let first = name.substring(from: 0) // output: "John Doe"
let last = name.substring(from: 5) // output: "Doe"

// substring(to: )
let beginning = name.substring(to: 6) // output: "John D"
let end = name.substring(to: 3) // output: "Joh"
```

## गहराई में जाओ

Substring का इस्तेमाल बहुत सालों से किया जाता रहा है, और कई अन्य programming भाषाओं में भी उपलब्ध है। उपलब्ध विकल्पों में, `substring` सबसे अधिक प्रभावी तरीका है, और भाषा के उत्पादकों की ओर से इसे समर्थन किया गया है। यह भी उल्लेखनीय है कि `substring` के लिए आपको string का starting index और ending index दोनों ही देने होते हैं, जो कि indexing में ध्यान रखने की जरूरत होती है।

## अन्य संबंधित लिंक

- [Swift Official Documentation on Substrings](https://developer.apple.com/documentation/swift/string#2975925)
- [Substrings Explained in Hindi](https://www.geeksforgeeks.org/swift-substring/)
- [Understanding String Indices in Swift](https://www.hackingwithswift.com/example-code/strings/what-are-indices-in-swift)