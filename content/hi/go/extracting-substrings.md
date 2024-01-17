---
title:                "उपस्थित सबस्ट्रिंग्स को निकालना"
html_title:           "Go: उपस्थित सबस्ट्रिंग्स को निकालना"
simple_title:         "उपस्थित सबस्ट्रिंग्स को निकालना"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/extracting-substrings.md"
---

{{< edit_this_page >}}

आज के दौर में, प्रोग्रामिंग जगत में विभिन्न भाषाओं में कोडिंग होती है। Go भी उनमें से एक है। इस भाषा की विशेषताएं, सुरक्षित एवं त्वरित आवेदनों को सुनिश्चित करती हैं। इस लेख में, हम बात करेंगे कि कैसे Go में substrings को निकाला जाता है। यह क्या है और क्यों हम इसे करते हैं? चलिए देखते हैं।

## What & Why?
Substring नाम से खुशी मिलती है। इसमें, एक string से हम एक छोटा string निकालते हैं। उसे कहा जाता है substring। प्रोग्रामिंग में, यह काफी ज्यादा उपयोग किया जाता है। जैसे कि, अक्सर हमारों को स्ट्रिंग हासिल होती है जिसे हमें अलग अलग भागों में बांटना होता है। सुनिश्चित होना होता है कि हमें इसमें गलती ना हो जाए। इसीलिए हम substrings का उपयोग करते हैं।

## How to:
चलिए, सामने दिए गए उदाहरण के माध्यम से देखते हैं कि कैसे Go में substrings निकाले जाते हैं।

```Go
// यह हमारा string है
s := "नमस्ते दुनिया!"
// हमें इसमें से पहले ४ अक्षर लेना है
substring := s[0:4]
// substring प्रिंट करे
fmt.Println(substring)

// Output: नमस
```

जैसे आप देख सकते हैं, हमने ```s``` से चौधा अक्षरों को निकाल लिया है और उसे ```substring``` में स्टोर किया है। अब हमने इसे प्रिंट किया है और केवल पहले चार अक्षर दिखाए।

## Deep Dive:
Substring का इतिहास देखे तो पता चलता है कि यह सबसे पहले प्रोग्रामिंग भाषाओं में COBOL ने किया गया था। लेकिन जैसे-जैसे भाषाएं बदलती गई, substring का तरीका भी बदल गया। अन्य भाषाओं में C और C++ के अलावा Python भी substring का उपयोग करते हैं।

## See Also:
जैसा कि हमने देखा, substrings का उपयोग स्ट्रिंग का एक छोटा हिस्सा निकालने के लिए किया जाता है। अगर आपको इस टॉपिक पर और भी जानकारी चाहिए तो आप नीचे दिए गए लिंक्स पर क्लिक कर सकते हैं:

1. [The official Go Documentation](https://golang.org/)
2. [A comprehensive tutorial on Go](https://www.tutorialspoint.com/go/index.htm)
3. [A detailed explanation of substrings in Go](https://golangdocs.com/get-certain-parts-of-strings-in-golang)
4. [More coding examples for substrings in Go](https://www.callicoder.com/golang-strings-and-string-functions/)